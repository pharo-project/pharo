I am a namespace for refactoring operations.
I collect changes during refactoring and provide an environment for resolving named entities like
classes, method senders or implementors. 

The default environment for a namespace contains all classes in the system, but you can create 
an instance of me with a scoped environment to apply the refactoring operations only on entities in 
that environment.

Resolving named entities includes classes and methods changed or added during refactoring, before
actually applying this changes to the system. For this, I create model classes - RBClass and RBMetaclass - modeling real classes.

The changes that will be applied to the system are collected as refactoring changes (RBRefactoryChange).

Example: 
This creates a new namespace on the default environment (all system classes):
 model := RBNamespace onEnvironment: RBBrowserEnvironment new.

This creates a Refactoring operation for adding a class, with me as the namespace model:
    refactoring := RBAddClassRefactoring
        model: model
        addClass: #SomeClass
        superclass: #Object
        subclasses: {}
        category: #Category.

This will do a "dry run", only collect the changes of this refactoring:
    refactoring primitiveExecute.

Now can ask me for this list of changes:
   model changes.
this acutally gives a composite refactory change that holds a list of all single refacoring changes.

Note, some of my methods for querying method senders and implementors don't work well for Trait methods, because we don't have yet modeling classes for Traits and some refactoring operations don't support refactorings on trait methods.
