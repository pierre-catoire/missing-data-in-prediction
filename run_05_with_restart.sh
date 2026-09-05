#!/usr/bin/env bash
################################################################################
## Auto-restart wrapper for 05_validation_analysis.R
##
## Why this exists: validation_functions.R's SIR-based samplers
## (sample_x1_mc()/sample_x1_mc_y()) call stop() if the importance weights
## for a given grid point degenerate to all-zero. That is rare, but on an
## unattended 24h run it would otherwise kill the whole process partway
## through. Because 05_validation_analysis.R already checkpoints every
## completed grid point to disk (output/validation_analysis/raw/...) and
## skips them on relaunch, simply re-invoking the script after any crash is
## safe and cheap: only the single point that was in flight at the moment
## of the crash is ever redone.
##
## This wraps the WHOLE script (not just one point), so it also recovers
## from anything else that might kill the R process on a VPS left running
## for a day: an OOM kill, a transient disk hiccup, etc.
##
## Usage (run from the repository root, i.e. the same directory you'd
## normally run `Rscript 05_validation_analysis.R` from):
##
##   nohup ./run_05_with_restart.sh > validation_analysis_wrapper.log 2>&1 &
##
## Each individual attempt's full R output also goes to its own file
## (validation_analysis_attempt_<n>.log), so you can inspect exactly what
## happened on a given crash without scrolling through the whole history.
##
## To stop it between attempts (e.g. you want to abort the run cleanly):
##   touch STOP_05
## (checked after each attempt finishes/crashes, before the next launch).
## To stop it immediately: kill the nohup'd process directly.
################################################################################

set -uo pipefail

SCRIPT="05_validation_analysis.R"
MAX_ATTEMPTS=50    # generous ceiling; a real non-transient bug (e.g. a bad
                    # config change) shouldn't be allowed to loop forever
                    # and burn VPS time
SLEEP_SECONDS=60    # pause before restarting; avoids hammering the machine
                    # in a tight crash loop, and gives a transient issue
                    # (e.g. brief disk/memory pressure) time to clear
STOP_FILE="STOP_05"

if [ ! -f "$SCRIPT" ]; then
  echo "ERROR: $SCRIPT not found in $(pwd). Run this from the repository root." >&2
  exit 1
fi

attempt=0
while [ "$attempt" -lt "$MAX_ATTEMPTS" ]; do
  attempt=$((attempt + 1))
  ts=$(date '+%Y-%m-%d %H:%M:%S')
  attempt_log="validation_analysis_attempt_${attempt}.log"
  echo "[$ts] Attempt $attempt/$MAX_ATTEMPTS: launching $SCRIPT (output: $attempt_log)"

  Rscript "$SCRIPT" > "$attempt_log" 2>&1
  status=$?

  ts=$(date '+%Y-%m-%d %H:%M:%S')

  if [ "$status" -eq 0 ]; then
    echo "[$ts] $SCRIPT completed successfully on attempt $attempt. Done."
    exit 0
  fi

  echo "[$ts] $SCRIPT exited with status $status on attempt $attempt. Last 20 lines of its log:"
  tail -n 20 "$attempt_log"

  if [ -f "$STOP_FILE" ]; then
    echo "[$ts] $STOP_FILE present; not restarting. Remove it and re-run this wrapper to resume."
    exit 1
  fi

  if [ "$attempt" -ge "$MAX_ATTEMPTS" ]; then
    echo "[$ts] Reached MAX_ATTEMPTS=$MAX_ATTEMPTS without a clean completion; giving up."
    echo "[$ts] Inspect validation_analysis_attempt_*.log to see what kept failing."
    exit 1
  fi

  echo "[$ts] Restarting in ${SLEEP_SECONDS}s -- already-checkpointed points will be skipped, not redone."
  sleep "$SLEEP_SECONDS"
done
