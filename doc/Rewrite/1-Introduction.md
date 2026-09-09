This documentation is based on [https://thepharo.dev/2021/12/09/what-is-rbparsetreesearcher/](https://thepharo.dev/2021/12/09/what-is-rbparsetreesearcher/)
written by Aless Hosry. Pharo thanks her for this nice material.


Imagine that you want to find a specific expression and that you want to find it in the complete system. How many classes would you have to look for? 
How can you be sure that you did not miss any class and being sure that you won’t be frustrated because of the number of issues thrown on compilation or 
execution? In addition imagine other scenario where you want to transform that expression into another one.

Changing code, removing, or replacing deprecated methods is costly for a developer by doing it manually instead of using an automated feature.

We explain how to find a specific piece of code we may look for inside a Pharo program, and make it easy for the developers to 
deal with pattern matching and `ASTParseTreeSearcher` class.

You can find tools to help you in the Library menu of Pharo. 

- Expression finder: It lets you give a matching expression and show you the code that matches it
- MatchTool originally developed by Y. Timchuk: It lets you understand how expressions are matched
- Rewrite rule editor: It lets you write rules that will transform the code using two matching expressions one for the target and one for the resulting code.
These tools have been maintained and redesigned by Sebastian Jordan-Montano.

We explain some fundamental definitions in the following sections:

- Pattern code description
- ASTParseTreeSearcher description
- ASTParseTreeSearcher examples with pattern code
