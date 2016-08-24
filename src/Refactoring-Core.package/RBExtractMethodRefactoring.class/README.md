I am a refactoring for creating a method from a code fragment.

You can select an interval of some code in a method and call this refactoring to create a new method implementing that code and replace the code by calling this method instead. 
The new method needs to have as many arguments as the number of (temp)variables, the code refers to.

The preconditions are quite complex. The code needs to be parseable valid code. 