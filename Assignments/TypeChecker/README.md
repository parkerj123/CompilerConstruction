# compiler-assignments

Clone the repository into the repo that you use for the solutions of the assignments for the course on compiler construction.

## Assignment Typechecker

The assignment is detailed [here](http://www.grammaticalframework.org/ipl-book/assignments/assignment2/assignment2.html).

To implement the typechecker I suggest that you use one of the following languages: Java, Scala, or Haskell.

To get you started Samuel Balco implemented the type checker in Haskell and Scala  and we can offer you template files in which you fill in the missing cases in the typechecker template file provided. 

- Haskell has the advantage that the code is the closest to the specification. One reason for this is that Haskell supports algebraic data types and pattern matching while Java does not. A disadvantage of Haskell is that it requires some programming language concepts that you are likely to not have seen before, such as monads which are needed for error handling.

- Scala has the advantage that it is close to Java but also incorporates features from functional programming similar to Haskell. In particular Scala has algebraic data types and pattern matching. We provide two implementations of the typechecker. Typechecker.scala was obtained by translating (by hand, not automatically) the Haskell typechecker into the object oriented language Scala. TypecheckerAlt.scala was obtained as a further modification that uses a method of error handling akin to the one of Java.

- Java has the advantage that it is the most familiar language to you. It also will give you the full experience as you are asked to produce the Java files you need using bnfc. If you have some previous experience with Java you should be able to see how to proceed from there by looking at Chapter 4 of the book and the Scala template file for the typechecker.


## Assignment Interpreter (tbc)






