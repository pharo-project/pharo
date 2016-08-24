I am a refactoring for removing classes. 

My precondition verifies that the class name exists in this namespace and the class has no references, resp. users, if this is used to remove a trait.

If this class is "empty" (has no methods and no variables), any subclass is reparented to the superclass of this class. It is not allowed to remove non-empty classes when it has subclasses.