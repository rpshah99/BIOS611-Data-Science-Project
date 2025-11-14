# BIOS611-Data-Science-Project
This is my data science project for my BIOS611 class. My goal in this project 
is to answer several research questions associated with the EROWID data set.

This data set was scraped from the EROWID web page. The EROWID web page contains
information on people taking various drugs, which ranges from plants, chemicals, 
etc., and recording their experiences while on their trips.

The research questions I'm taking are:
What words are most often associated with the summaries from the those on drug
trips from taking DMT?

Does the experience change depending on how DMT was administered?

Do the drug trips change as the DMT doses increase?

The first step that I'm taking is tidying my data that was scraped from the
EROWID web page. This involves joining the summaries and doses data raw files.
Next, the NAs were removed from the joined variable and was renamed "cleaned_joined
_summaries_dose".


