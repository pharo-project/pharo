"
I am a subclass of Add Method Refactoring that loosens preconditions.

Add Method has the breaking change that methods should not be defined in the target class or its superclasses.
Add Override Method allows Refactoring to create a method in a class that can override a superclass method, but it does not allow overwrite of a method that is directly defined in that class.

"
Class {
	#name : 'RBAddOverrideMethodTransformation',
	#superclass : 'RBAddMethodRefactoring',
	#category : 'Refactoring-Core-Refactorings',
	#package : 'Refactoring-Core',
	#tag : 'Refactorings'
}

{ #category : 'preconditions' }
RBAddOverrideMethodTransformation >> breakingChangePreconditions [

	^ { (RBCondition definesSelector: transformation selector in: class) not }
]
