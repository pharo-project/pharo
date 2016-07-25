I am the base class of RBClass and RBMetaclass implementing the common behavior for creating and accessing class and metaclass elements in a refactoring namespace. 

My subclass instances are created in a refactoring namespace when querying the environment for existing 
classes (or metaclasses) and when creating or changing classes during refactoring operations.

RBClass and RBMetaclass are used to model existing, changed or newly added classes before actually applying the refactoring change to the system. 
You can query the class name,  defined methods and  variables. 
They provide a similar interface like the real classes, but only the part that is necessary  for the 
refactoring framework.

They shouldn't be directly used and always be a part of a refacotoring namespace - the model.

Likewise this classes, my methods aren't real methods but models (RBMethod) that representing a real methods.

Changes for my methods during refactoring are stored in a list of
newMethods and removedMethods.
Like a real class I referre to my superclass and subclass, which again are actually RBClasses.
