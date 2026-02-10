ENGINE := lualatex
LATEX_OPTS := -interaction=nonstopmode -halt-on-error

MAIN_OUT := ../../output/main/figures
SEC_OUT  := ../../output/secondary/figures

MAIN_TEX := figure_main.tex
SEC_TEX  := figure_secondary.tex

# =================================================
# Default target
# =================================================

all: main secondary

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
# Cleanup
# =================================================

clean:
	rm -f $(MAIN_OUT)/*.aux $(MAIN_OUT)/*.log
	rm -f $(SEC_OUT)/*.aux $(SEC_OUT)/*.log

.PHONY: all main secondary clean
