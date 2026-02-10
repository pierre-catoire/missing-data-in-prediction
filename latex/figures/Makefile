ENGINE := lualatex
LATEX_OPTS := -interaction=nonstopmode -halt-on-error
OUTDIR := ../../output/main/figures
TEX := figure_main.tex

all: \
	$(OUTDIR)/figure_overall_mse.pdf \
	$(OUTDIR)/figure_overall_msepomu.pdf \
	$(OUTDIR)/figure_overall_msepomc.pdf \
	$(OUTDIR)/figure_complete_mse.pdf \
	$(OUTDIR)/figure_complete_msepomu.pdf \
	$(OUTDIR)/figure_complete_msepomc.pdf \
	$(OUTDIR)/figure_incomplete_mse.pdf \
	$(OUTDIR)/figure_incomplete_msepomu.pdf \
	$(OUTDIR)/figure_incomplete_msepomc.pdf

$(OUTDIR):
	mkdir -p $(OUTDIR)

# =========================
# OVERALL
# =========================

$(OUTDIR)/figure_overall_mse.pdf: | $(OUTDIR)
	$(ENGINE) $(LATEX_OPTS) -output-directory=$(OUTDIR) "\
\def\grp{overall}\
\def\metric{mse}\
\def\YMIN{0.5}\
\def\YMAX{2}\
\def\YLABEL{{Mean Squared Error}}\
\input{$(TEX)}"
	mv $(OUTDIR)/figure_main.pdf $@

$(OUTDIR)/figure_overall_msepomu.pdf: | $(OUTDIR)
	$(ENGINE) $(LATEX_OPTS) -output-directory=$(OUTDIR) "\
\def\grp{overall}\
\def\metric{msepomu}\
\def\YMIN{0}\
\def\YMAX{1.5}\
\def\YLABEL{{MSEP – Oracle MU}}\
\input{$(TEX)}"
	mv $(OUTDIR)/figure_main.pdf $@

$(OUTDIR)/figure_overall_msepomc.pdf: | $(OUTDIR)
	$(ENGINE) $(LATEX_OPTS) -output-directory=$(OUTDIR) "\
\def\grp{overall}\
\def\metric{msepomc}\
\def\YMIN{0}\
\def\YMAX{1.5}\
\def\YLABEL{{MSEP – Oracle MC}}\
\input{$(TEX)}"
	mv $(OUTDIR)/figure_main.pdf $@

# =========================
# COMPLETE
# =========================

$(OUTDIR)/figure_complete_mse.pdf: | $(OUTDIR)
	$(ENGINE) $(LATEX_OPTS) -output-directory=$(OUTDIR) "\
\def\grp{complete}\
\def\metric{mse}\
\def\YMIN{0}\
\def\YMAX{5}\
\def\YLABEL{{Mean Squared Error}}\
\input{$(TEX)}"
	mv $(OUTDIR)/figure_main.pdf $@

$(OUTDIR)/figure_complete_msepomu.pdf: | $(OUTDIR)
	$(ENGINE) $(LATEX_OPTS) -output-directory=$(OUTDIR) "\
\def\grp{complete}\
\def\metric{msepomu}\
\def\YMIN{0}\
\def\YMAX{3}\
\def\YLABEL{{MSEP – Oracle MU}}\
\input{$(TEX)}"
	mv $(OUTDIR)/figure_main.pdf $@

$(OUTDIR)/figure_complete_msepomc.pdf: | $(OUTDIR)
	$(ENGINE) $(LATEX_OPTS) -output-directory=$(OUTDIR) "\
\def\grp{complete}\
\def\metric{msepomc}\
\def\YMIN{0}\
\def\YMAX{3}\
\def\YLABEL{{MSEP – Oracle MC}}\
\input{$(TEX)}"
	mv $(OUTDIR)/figure_main.pdf $@

# =========================
# INCOMPLETE
# =========================

$(OUTDIR)/figure_incomplete_mse.pdf: | $(OUTDIR)
	$(ENGINE) $(LATEX_OPTS) -output-directory=$(OUTDIR) "\
\def\grp{incomplete}\
\def\metric{mse}\
\def\YMIN{0}\
\def\YMAX{5}\
\def\YLABEL{{Mean Squared Error}}\
\input{$(TEX)}"
	mv $(OUTDIR)/figure_main.pdf $@

$(OUTDIR)/figure_incomplete_msepomu.pdf: | $(OUTDIR)
	$(ENGINE) $(LATEX_OPTS) -output-directory=$(OUTDIR) "\
\def\grp{incomplete}\
\def\metric{msepomu}\
\def\YMIN{0}\
\def\YMAX{3}\
\def\YLABEL{{MSEP – Oracle MU}}\
\input{$(TEX)}"
	mv $(OUTDIR)/figure_main.pdf $@

$(OUTDIR)/figure_incomplete_msepomc.pdf: | $(OUTDIR)
	$(ENGINE) $(LATEX_OPTS) -output-directory=$(OUTDIR) "\
\def\grp{incomplete}\
\def\metric{msepomc}\
\def\YMIN{0}\
\def\YMAX{3}\
\def\YLABEL{{MSEP – Oracle MC}}\
\input{$(TEX)}"
	mv $(OUTDIR)/figure_main.pdf $@

clean:
	rm -f $(OUTDIR)/*.aux $(OUTDIR)/*.log
