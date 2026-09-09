"
Rename a class but making sure that existing clients not in the system can be migrated later on:
The idea is that 
	the class A is renamed into Anew
	it gets a subclass A that is deprecated. 
	
This way client code using A will be able to load and get all the behavior of Anew. 


Usage:
```
| transformation |
transformation := (RBRenameAndDeprecateClassTransformation 
				rename: #DANode
				to: #DANodePresenter)
			generateChanges.
(StRefactoringPreviewPresenter for: transformation) open
```


"
Class {
	#name : 'RBRenameAndDeprecateClassTransformation',
	#superclass : 'RBCompositeTransformation',
	#instVars : [
		'className',
		'newClassName'
	],
	#category : 'Refactoring-Transformations',
	#package : 'Refactoring-Transformations'
}

{ #category : 'api' }
RBRenameAndDeprecateClassTransformation class >> model: aRBModel rename: aClassName [

	^ self new
		model: aRBModel;
		className: aClassName
		yourself
]

{ #category : 'api' }
RBRenameAndDeprecateClassTransformation class >> model: aRBModel rename: aClassName to: aNewClassName [

	^ self new
		model: aRBModel;
		className: aClassName
		newClassName: aNewClassName;
		yourself
]

{ #category : 'api' }
RBRenameAndDeprecateClassTransformation class >> rename: aClassName [

	^ self new
		className: aClassName
]

{ #category : 'api' }
RBRenameAndDeprecateClassTransformation class >> rename: aClassName to: aNewClassName [

	^ self new
		className: aClassName
		newClassName: aNewClassName
]

{ #category : 'preconditions' }
RBRenameAndDeprecateClassTransformation >> applicabilityPreconditions [

	| class |
	class := self model classNamed: className.
	^ {
		  (ReClassesExistCondition new classes: { class name -> class } asDictionary).
		  (ReClassesAreNotMetaClassCondition new classes: { class }).
		  (ReNameIsGlobalCondition new
			   model: self model
			   className: newClassName) not.
		  (ReValidClassNameCondition new className: newClassName) }
]

{ #category : 'executing' }
RBRenameAndDeprecateClassTransformation >> buildTransformations [

	^ transformations ifNil: [
		transformations := OrderedCollection
			with: (ReRenameClassRefactoring
						model: self model
						rename: className to: newClassName)
			with: ((RBInsertNewClassTransformation
					   model: self model
						className: className)
						superclass: newClassName asSymbol;
						subclasses: #();
						packageName: (self model classNamed: className) packageName)
		with: (RBDeprecateClassTransformation
						model: self model
						className: className)]
]

{ #category : 'api' }
RBRenameAndDeprecateClassTransformation >> className: aClassName [

	className := aClassName asSymbol 
]

{ #category : 'api' }
RBRenameAndDeprecateClassTransformation >> className: aClassName newClassName: aNewClassName [

	className := aClassName asSymbol.
	newClassName := aNewClassName asSymbol 
]

{ #category : 'api' }
RBRenameAndDeprecateClassTransformation >> newClassName: aNewClassName [

	newClassName := aNewClassName asSymbol 
]

{ #category : 'private' }
RBRenameAndDeprecateClassTransformation >> tmpName [
	"return the hidden name of the subclass"

	^ (#TmpSubclass,className) asSymbol
]
