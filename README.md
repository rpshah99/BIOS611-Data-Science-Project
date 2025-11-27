# BIOS611-Data-Science-Project

docker build --platform=linux/amd64 . -t bios611
docker run --platform=linux/amd64 -v $(pwd):/home/rstudio/work -p 8787:8787 -it bios611

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

Is there a difference between the top words associated with DMT trips and 
Salvia divinorum trips?

The measurement that I'm using the answer these questions are the word count of
the summaries from each drug trip.

The first step that I'm taking is tidying my data that was scraped from the
EROWID web page. This involves joining the summaries and doses data raw files.
This also includes removing the NAs from the file, cleaning the data set
of punctuation, getting the word counts from the summaries, removing the stop
words and putting them into a variable labeled "top words DMT."

After cleaning the data set I put these top words into a bubble scatter plot
that includes increasing bubbles based on the frequency of the word. This 
plot is labeled "Top Words Associated with DMT Trips."

The next step that I took was to tidy my data in order to make a heat map based 
on the frequency of the words that appeared in DMT trips. I repeated the tidying
steps I took in making the bubble scatter plot in order to make this heat map.
This plot is a similar visualization of the bubble scatter plot but in the heat
map format. I believe that the bubble scatter plot was a better visual 
representation of the top words that are associated with DMT trips.

After this, I want to ask the question if the dosage changes the summaries that
are associated with DMT trips. First, I tidied the data by including the dosage
of DMT that the participant took, removing punctuation and getting the word
count of each summary associated with each dose. Then I plotted this in a 3D 
scatter plot in order to see the differences between the dosage and the word 
count.

Next, I wanted to focus on if the method of consumption of DMT changes the word 
count of the summaries. To do this, I first tidied the data in order to get
a word count associated with each DMT trip remove the columns that didn't 
include the method of consumption and the word count of the summaries. After 
that I mapped this information on a box plot in order to compare the word
count of the summaries of the DMT trips based on the method by which DMT
was consumed.

The last step I took was to ask if the top words changed if someone took
Salvia divinorum rather than DMT. I ran the same code that I did for DMT
but I used it on the Salvia divinorum data set. I found that most of the top 
words were similar but there were some words that differed.



