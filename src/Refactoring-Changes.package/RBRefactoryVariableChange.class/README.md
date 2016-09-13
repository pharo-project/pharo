I am the baseclass for all refactoring changes for all kind of variable changes, adding or removing class,  instance variables and pool variables.
I only hold the name of the actual variable. The concrete kind of variable and how the defintion for adding this variable looks like, is implemented
by my subclasses.