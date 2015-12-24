#Create samples that contain 1% of the elements of the original files, randomly.

perl -ne 'print if (rand() < .01)' en_US.twitter.txt > twitter_sample.txt
perl -ne 'print if (rand() < .01)' en_US.news.txt > news.txt
perl -ne 'print if (rand() < .01)' en_US.blogs.txt > blogs_sample.txt