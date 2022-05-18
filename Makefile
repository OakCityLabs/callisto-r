.PHONY: save_env load_env test single_test

# Makefile variables
ENVNAME=callisto_r
ENVFILE=environment.yml
CONDADIR=`conda info --base`

MIN_CODE_COVERAGE=60
TEST_ARGS=--cov-fail-under=$(MIN_CODE_COVERAGE)

help:
	@echo "  make save_env				save current development environment using conda to $(ENVFILE)"
	@echo "  make load_env				create a development environment using conda from $(ENVFILE) (deletes current env definition)"
	@echo "  make test					run tests with pytest"
	@echo "  make single_test			run a single test with pytest"

save_env:
	conda env export --no-builds > $(ENVFILE)

load_env:
	bash -lc "conda deactivate ; conda remove env --name $(ENVNAME) --all -y ; conda env create -f $(ENVFILE)"
	@echo Activate the new environment with
	@echo "  " conda activate $(ENVNAME)

check:
	R CMD check

build:
	R CMD BUILD . && R CMD CHECK `ls -t . | head -n1`

test:
	R -e "devtools::test()"
