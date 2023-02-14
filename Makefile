.PHONY: save_env load_env test

# Makefile variables
ENVNAME=callisto_r
ENVFILE=environment.yml
CONDADIR=`conda info --base`

help:
	@echo "  make save_env				save current development environment using conda to $(ENVFILE)"
	@echo "  make load_env				create a development environment using conda from $(ENVFILE) (deletes current env definition)"
	@echo "  make build					build and check resulting package with R CMD check"
	@echo "  make test					run tests"

save_env:
	conda env export --no-builds > $(ENVFILE)

load_env:
	bash -lc "conda deactivate ; conda remove env --name $(ENVNAME) --all -y ; conda env create -f $(ENVFILE)"
	@echo Activate the new environment with
	@echo "  " conda activate $(ENVNAME)

build:
	R CMD BUILD . && R CMD CHECK `ls -t . | head -n1`

test:
	R -e "devtools::test()"

make_docs:
	Rscript -e "roxygen2::roxygenise()"
