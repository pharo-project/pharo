I am a refactoring for removing classes. 

My precondition verifies that the class name exists and the class has no references, resp. users, if this is used to remove a trait.
If this class has subclasses, they are reparent to the superclass of that class.