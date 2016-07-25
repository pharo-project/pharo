I represent a method for the refactoring framework.

I am used by RBClass / RBMetaclass resp. RBAbstractClass for methods created or changed during a refactoring operation.
I represent the method with a selector, source and if I am created from an existing method, its 
CompiledMethod. 
I know my method class (a RBClass or RBMetaclass). 
You should not directly create instances of me but query or create a method from a RBClass.

I only implement a small part of  CompiledMethod interface, that is used for refactoring operations, like
querying symbols, literals or the whole method source.
