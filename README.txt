# proglang3

Names: Ryan Tedaldi & Peter Simone
Things to Note: 
    Part 1: For part 1, besides the single line we needed to write in nlp_parse, the entirety of the code
    was written in the form of DCG Grammar. Most of the work that needed to be done was just writing similar
    rules to the ones in the assignment pdf. One of our key issues in part 1 was with the Complex NLP test
    case. Where we could not figure out the edge case. After writing out the expected output, we were able
    to discover that the question involved double-nested Ands, which our code did not account for. After
    some tinkering, we were able to get full points on that part of the assignment. 

    Part 2: For part 2, we had to write a series of functions such that the queries created from part 1 could be
    processed into a readable format with all of the information from the facts.pl file. Our major issue in part 2
    were the additional tests on Submitty that we were not able to get to pass. Similarly to part 1, it was difficult
    to get the Complex Constraint Satisfaction, as many of the queries beared resembalance to the Complex NLP tests.
    Knowing about the double-nested ands/ors, we were able to fix our code to get the 5 points on the autograder.