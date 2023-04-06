# ChildhoodCancerDataInitiative-Template_ExampleR
This script will take a CCDI metadata submission template file and create a fake example data set.

To run the script on a CCDI template, run the following command in a terminal where R is installed for help.

```
Rscript --vanilla CCDI-Template_ExampleR.R -h
```

```
Usage: CCDI-Template_ExampleR.R [options]

CCDI-Template_ExampleR v1.0.0

Options:
	-t CHARACTER, --template=CHARACTER
		dataset template file, CCDI_submission_metadata_template.xlsx

	-n NUMBER, --number=NUMBER
		number of entries to be created, default is 20

	-h, --help
		Show this help message and exit
```

To run this script, please obtain a [CCDI Submission Template](https://github.com/CBIIT/ccdi-model/tree/main/metadata-manifest).
