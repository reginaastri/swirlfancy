## heavy.R: sequencing logic relegated to states 

Sequencing logic is relegated to individual units of instruction, represented as "states" which belong to various S3 classes. States return instructions to the controlling callback. Instructions depend on a state's class and user behavior. At the moment only two classes are implemented, a default and a class for question/command units.

"Heavy.R" is limited to Module 2 of Data Analysis, represented as a csv file with a few manual alterations. Features such as support for multiple users, or tracking user progress are not implemented.

An annotated version of Hadley's frndly.R code, and an earlier demo are included for reference. To run either, source the relevant file, type "hi()" to begin, "nxt()" to advance, and "bye()" to quit.
