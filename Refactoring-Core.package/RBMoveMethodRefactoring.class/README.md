I am a refactoring for moving a method from the class to one of its instance variable objects.

Moving a method moves it implementation to one or more classes and replaces the implementation in the original method by a delegation to one of the classes instance variable. 

I expect an option for selecting the type (classes) to which this method should be added.
A role typer RBRefactoryTyper is used to guess the possible classes used for this instance variables.
And an option for requesting the new method selector.

For all selected classes a method implementing the original method is created, and if the original code uses some references to self, a parameter needs to be added to provided the former implementor.

For example, moving the method #isBlack from class Color to its instvar #rgb for the type "Integer" creates a method 
Integer>>#isBlack
 ^ self = 0

and changes Colors implementation from: 
Color>>#isBlack
   ^ rgb = 0
to: 
Color>>#isBlack
   ^ rgb isBlack
