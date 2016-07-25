I am a refactoring operation for moving a class and its subclasses to a new super class.

You can choose which of the original childclasses should become now siblings.

For example,  we can generate a new Superclass for ClassS in
Object >> ClassP >> ClassS
Object >> ClassP >> ClassS >> ClassC1
Object >> ClassP >> ClassS >> ClassC2
Object >> ClassP >> ClassS >> ClassC3

and choose to move ClassC2 and ClassC3 to the new superclass - ClassNewP.

Object >> ClassP >> ClassNewP >> ClassS
Object >> ClassP >> ClassNewP >> ClassS >> ClassC1
Object >> ClassP >> ClassNewP >> ClassC2
Object >> ClassP >> ClassNewP >> ClassC3

Any method and instance variables,  defined in ClassS and used by the new siblings of ClassS are pushed up to the new superclass.

