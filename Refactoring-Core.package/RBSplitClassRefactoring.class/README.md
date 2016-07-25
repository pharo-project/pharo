I am a refactoring for extracting instance variables to a new class.

You can choose which instance variables should be moved into the new class. The new class becomes an instvar of the 
original class and every reference to the moved variables is replaced by a accessor call.

My precondition verifies that the new instance variable for the splitted class is a valid variable name and not yet used in this class or its hierarchy.