# ADVANCE
A study of the co-authorship networks of female researchers in the Microsoft Academic Graph Database

## Getting Started
The code provided below does not automatically run from start to finish. Instead, I outline the order of execution and provide an explanation of what each file does.

### Prerequisites
You need access to the Microsoft Academic Graph dataset. I cannot find the download link anymore, but here is the original announcement [https://www.microsoft.com/en-us/research/blog/announcing-the-microsoft-academic-graph-let-the-research-begin/].

Most of the code also assumes that you have an instance of Neo4j installed on some server. I tried to ensure that all calls to the database were off a variable but I could have easily missed an instance or two. You may have to chase down the input on certain calls.

If you want to know how I got the MAG data into a Neo4j database, visit here: [http://www.markcosta.net/load-the-microsoft-academic-graph-into-neo4j/] and here: [https://github.com/markrcosta/MAGtoNEO4J].

If you run the scripts on your own server, you will also need to subscribe to [http://genderize.io]

All of the code in this repository was written in R, using Microsoft's R Open.

One thing to note - I make extensive use of parallelization at some points. I was working with a system with 12 core and 192 gb of RAM. If you are working with less, I suggest tinkering with the doParallel settings.

## Files
1. authors.R, which generates files for unique first names in the database by starting letter.
2. genderize.R: which queries [http://www.genderize.io] for the predicted gender.

A number of names came back ungenderized because the given and surnames were transposed. Run the following scripts to genderize the names in the second position for names in the first position that had no gender associated with them:

3. prepLastName.R: creates files with unique second literal for each letter
4. lastNameGenderize.R: makes the api call and stores gender in separate files
5. authorIDLastName.R: merges the gender returned with the first literal and the authorID


