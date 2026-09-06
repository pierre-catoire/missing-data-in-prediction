ENGINE := lualatex
LATEX_OPTS := -interaction=nonstopmode -halt-on-error

MAIN_OUT := ../../output/main/figures
SEC_OUT  := ../../output/secondary/figures
VALCONS_OUT := ../../output/validation_analysis/figures

MAIN_TEX := figure_main.tex
SEC_TEX  := figure_secondary.tex
VALCONS_TEX := figure_validation_consistency.tex
THEO_TEX := figure_theoretical_risks.tex

# =================================================
# Default target
# =================================================

all: main secondary theoretical_risks validation_consistency

# =================================================
# MAIN FIGURES (9 figures)
# =================================================

main: \
	$(MAIN_OUT)/figure_overall_mse.pdf \
	$(MAIN_OUT)/figure_overall_msepomu.pdf \
	$(MAIN_OUT)/figure_overall_msepomc.pdf \
	$(MAIN_OUT)/figure_complete_mse.pdf \
	$(MAIN_OUT)/figure_complete_msepomu.pdf \
	$(MAIN_OUT)/figure_complete_msepomc.pdf \
	$(MAIN_OUT)/figure_incomplete_mse.pdf \
	$(MAIN_OUT)/figure_incomplete_msepomu.pdf \
	$(MAIN_OUT)/figure_incomplete_msepomc.pdf

$(MAIN_OUT):
	mkdir -p $(MAIN_OUT)

# ---- OVERALL ----

$(MAIN_OUT)/figure_overall_mse.pdf: | $(MAIN_OUT)
	$(ENGINE) $(LATEX_OPTS) -output-directory=$(MAIN_OUT) "\
\def\grp{overall}\
\def\metric{mse}\
\def\YMIN{0.5}\
\def\YMAX{2}\
\def\YLABEL{{Mean Squared Error}}\
\input{$(MAIN_TEX)}"
	mv $(MAIN_OUT)/figure_main.pdf $@

$(MAIN_OUT)/figure_overall_msepomu.pdf: | $(MAIN_OUT)
	$(ENGINE) $(LATEX_OPTS) -output-directory=$(MAIN_OUT) "\
\def\grp{overall}\
\def\metric{msepomu}\
\def\YMIN{0}\
\def\YMAX{1.5}\
\def\YLABEL{{MSEP – Oracle MU}}\
\input{$(MAIN_TEX)}"
	mv $(MAIN_OUT)/figure_main.pdf $@

$(MAIN_OUT)/figure_overall_msepomc.pdf: | $(MAIN_OUT)
	$(ENGINE) $(LATEX_OPTS) -output-directory=$(MAIN_OUT) "\
\def\grp{overall}\
\def\metric{msepomc}\
\def\YMIN{0}\
\def\YMAX{1.5}\
\def\YLABEL{{MSEP – Oracle MC}}\
\input{$(MAIN_TEX)}"
	mv $(MAIN_OUT)/figure_main.pdf $@

# ---- COMPLETE ----

$(MAIN_OUT)/figure_complete_mse.pdf: | $(MAIN_OUT)
	$(ENGINE) $(LATEX_OPTS) -output-directory=$(MAIN_OUT) "\
\def\grp{complete}\
\def\metric{mse}\
\def\YMIN{0}\
\def\YMAX{5}\
\def\YLABEL{{Mean Squared Error}}\
\input{$(MAIN_TEX)}"
	mv $(MAIN_OUT)/figure_main.pdf $@

$(MAIN_OUT)/figure_complete_msepomu.pdf: | $(MAIN_OUT)
	$(ENGINE) $(LATEX_OPTS) -output-directory=$(MAIN_OUT) "\
\def\grp{complete}\
\def\metric{msepomu}\
\def\YMIN{0}\
\def\YMAX{3}\
\def\YLABEL{{MSEP – Oracle MU}}\
\input{$(MAIN_TEX)}"
	mv $(MAIN_OUT)/figure_main.pdf $@

$(MAIN_OUT)/figure_complete_msepomc.pdf: | $(MAIN_OUT)
	$(ENGINE) $(LATEX_OPTS) -output-directory=$(MAIN_OUT) "\
\def\grp{complete}\
\def\metric{msepomc}\
\def\YMIN{0}\
\def\YMAX{3}\
\def\YLABEL{{MSEP – Oracle MC}}\
\input{$(MAIN_TEX)}"
	mv $(MAIN_OUT)/figure_main.pdf $@

# ---- INCOMPLETE ----

$(MAIN_OUT)/figure_incomplete_mse.pdf: | $(MAIN_OUT)
	$(ENGINE) $(LATEX_OPTS) -output-directory=$(MAIN_OUT) "\
\def\grp{incomplete}\
\def\metric{mse}\
\def\YMIN{0}\
\def\YMAX{5}\
\def\YLABEL{{Mean Squared Error}}\
\input{$(MAIN_TEX)}"
	mv $(MAIN_OUT)/figure_main.pdf $@

$(MAIN_OUT)/figure_incomplete_msepomu.pdf: | $(MAIN_OUT)
	$(ENGINE) $(LATEX_OPTS) -output-directory=$(MAIN_OUT) "\
\def\grp{incomplete}\
\def\metric{msepomu}\
\def\YMIN{0}\
\def\YMAX{3}\
\def\YLABEL{{MSEP – Oracle MU}}\
\input{$(MAIN_TEX)}"
	mv $(MAIN_OUT)/figure_main.pdf $@

$(MAIN_OUT)/figure_incomplete_msepomc.pdf: | $(MAIN_OUT)
	$(ENGINE) $(LATEX_OPTS) -output-directory=$(MAIN_OUT) "\
\def\grp{incomplete}\
\def\metric{msepomc}\
\def\YMIN{0}\
\def\YMAX{3}\
\def\YLABEL{{MSEP – Oracle MC}}\
\input{$(MAIN_TEX)}"
	mv $(MAIN_OUT)/figure_main.pdf $@

# =================================================
# SECONDARY FIGURES (3 figures, Scenario 5 only)
# =================================================

secondary: \
	$(SEC_OUT)/figure_secondary_mse.pdf \
	$(SEC_OUT)/figure_secondary_msepomu.pdf \
	$(SEC_OUT)/figure_secondary_msepomc.pdf

$(SEC_OUT):
	mkdir -p $(SEC_OUT)

$(SEC_OUT)/figure_secondary_mse.pdf: | $(SEC_OUT)
	$(ENGINE) $(LATEX_OPTS) -output-directory=$(SEC_OUT) "\
\def\metric{mse}\
\def\YLABEL{{Mean Squared Error}}\
\input{$(SEC_TEX)}"
	mv $(SEC_OUT)/figure_secondary.pdf $@

$(SEC_OUT)/figure_secondary_msepomu.pdf: | $(SEC_OUT)
	$(ENGINE) $(LATEX_OPTS) -output-directory=$(SEC_OUT) "\
\def\metric{msepomu}\
\def\YLABEL{{MSEP – Oracle MU}}\
\input{$(SEC_TEX)}"
	mv $(SEC_OUT)/figure_secondary.pdf $@

$(SEC_OUT)/figure_secondary_msepomc.pdf: | $(SEC_OUT)
	$(ENGINE) $(LATEX_OPTS) -output-directory=$(SEC_OUT) "\
\def\metric{msepomc}\
\def\YLABEL{{MSEP – Oracle MC}}\
\input{$(SEC_TEX)}"
	mv $(SEC_OUT)/figure_secondary.pdf $@

# =================================================
# THEORETICAL TARGET RISKS (1 figure: MU/MC x OP/CP/full, all 5 scenarios)
# =================================================

theoretical_risks: \
	$(VALCONS_OUT)/figure_theoretical_risks.pdf

$(VALCONS_OUT)/figure_theoretical_risks.pdf: | $(VALCONS_OUT)
	$(ENGINE) $(LATEX_OPTS) -output-directory=$(VALCONS_OUT) "\
\def\YMIN{0.5}\
\def\YMAX{3}\
\def\YLABEL{{Bayes target risk}}\
\input{$(THEO_TEX)}"

# =================================================
# VALIDATION CONSISTENCY FIGURES (4 figures)
# =================================================

validation_consistency: \
	$(VALCONS_OUT)/figure_validation_consistency_mu.pdf \
	$(VALCONS_OUT)/figure_validation_consistency_mc.pdf \
	$(VALCONS_OUT)/figure_validation_consistency_mu_withy.pdf \
	$(VALCONS_OUT)/figure_validation_consistency_mc_withy.pdf

$(VALCONS_OUT):
	mkdir -p $(VALCONS_OUT)

$(VALCONS_OUT)/figure_validation_consistency_mu.pdf: | $(VALCONS_OUT)
	$(ENGINE) $(LATEX_OPTS) -output-directory=$(VALCONS_OUT) "\
\def\family{mu}\
\def\variant{ordinary}\
\def\YMIN{0.9}\
\def\YMAX{2.75}\
\def\YLABEL{{Risk (MU target)}}\
\input{$(VALCONS_TEX)}"
	mv $(VALCONS_OUT)/figure_validation_consistency.pdf $@

$(VALCONS_OUT)/figure_validation_consistency_mc.pdf: | $(VALCONS_OUT)
	$(ENGINE) $(LATEX_OPTS) -output-directory=$(VALCONS_OUT) "\
\def\family{mc}\
\def\variant{ordinary}\
\def\YMIN{0.7}\
\def\YMAX{2.6}\
\def\YLABEL{{Risk (MC target)}}\
\input{$(VALCONS_TEX)}"
	mv $(VALCONS_OUT)/figure_validation_consistency.pdf $@

$(VALCONS_OUT)/figure_validation_consistency_mu_withy.pdf: | $(VALCONS_OUT)
	$(ENGINE) $(LATEX_OPTS) -output-directory=$(VALCONS_OUT) "\
\def\family{mu}\
\def\variant{withy}\
\def\YMIN{0.7}\
\def\YMAX{1.75}\
\def\YLABEL{{Risk (MU target, outcome included)}}\
\input{$(VALCONS_TEX)}"
	mv $(VALCONS_OUT)/figure_validation_consistency.pdf $@

$(VALCONS_OUT)/figure_validation_consistency_mc_withy.pdf: | $(VALCONS_OUT)
	$(ENGINE) $(LATEX_OPTS) -output-directory=$(VALCONS_OUT) "\
\def\family{mc}\
\def\variant{withy}\
\def\YMIN{0.65}\
\def\YMAX{2.7}\
\def\YLABEL{{Risk (MC target, outcome included)}}\
\input{$(VALCONS_TEX)}"
	mv $(VALCONS_OUT)/figure_validation_consistency.pdf $@

# =================================================
# Cleanup
# =================================================

clean:
	rm -f $(MAIN_OUT)/*.aux $(MAIN_OUT)/*.log
	rm -f $(SEC_OUT)/*.aux $(SEC_OUT)/*.log
	rm -f $(VALCONS_OUT)/*.aux $(VALCONS_OUT)/*.log

.PHONY: all main secondary theoretical_risks validation_consistency clean