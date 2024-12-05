# proglang3

Names: Ryan Tedaldi & Peter Simone
Things to Note: 
    Part 1: For part 1, besides the single line we needed to write in nlp_parse, the entirety of the code
    was written in the form of DCG Grammar. Most of the work that needed to be done was just writing similar
    rules to the ones in the assignment pdf. One of our key issues in part 1 was with the Complex NLP test
    case. Where we could not figure out the edge case. After writing out the expected output, we were able
    to discover that the question involved double-nested Ands, which our code did not account for. After
    some tinkering, we were able to get full points on that part of the assignment. 

    Part 2: As for part 2, we had a lot of troubles at first trying to understand how we needed to get the
    information from facts. Originally we had been using DCG grammar but after a trip to office hours,
    we were told to go back to the drawing board after some guidance. Overall, we definetely overthank the
    ask and by the end were able to figure it out. Some key things we did include a data sorting function
    which involved making a 6 digit number of the date (YYMMDD) and then comparing two numbers we made that way.
    