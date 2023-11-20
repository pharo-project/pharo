"
Rename a class but making sure that existing clients not in the system can be migrated later on:
The idea is that 
	the class A is renamed into Anew
	it gets a subclass A that is deprecated. 
	
This way client code using A will be able to load and get all the behavior of Anew. 


Usage:
transformation := (RBRenameAndDeprecateClassTransformation 
				rename: #DANode
				to: #DANodePresenter)
				transform. 
(ChangesBrowser changes: transformation model changes changes) open


"
Class {
	#name : 'RBRenameAndDeprecateClassTransformation',
	#superclass : 'RBCompositeTransformation',
	#instVars : [
		'className',
		'newClassName'
	],
	#category : 'Refactoring-Transformations-Model-Unused',
	#package : 'Refactoring-Transformations',
	#tag : 'Model-Unused'
}

{ #category : 'api' }
RBRenameAndDeprecateClassTransformation class >> model: aRBModel rename: aClassName to: aNewClassName [

	^ self new
		model: aRBModel;
		className: aClassName
		newClassName: aNewClassName;
		yourself
]

{ #category : 'api' }
RBRenameAndDeprecateClassTransformation class >> rename: aClassName to: aNewClassName [

	^ self new
		className: aClassName
		newClassName: aNewClassName
]

{ #category : 'executing' }
RBRenameAndDeprecateClassTransformation >> buildTransformations [

	^ transformations ifNil: [
		  | class |
		  class := self model classNamed: className.
		  transformations := OrderedCollection
			                     with: ((RBInsertNewClassTransformation className: self tmpName)
					                      superclass: className asSymbol;
					                      packageName: class packageName;
					                      tagName: class tagName;
					                      yourself)
			                     with: (RBRenameClassRefactoring rename: className to: newClassName)
			                     with: (RBRenameClassRefactoring rename: self tmpName to: className)
			                     with: (RBDeprecateClassTransformation class: className) ]
]

{ #category : 'api' }
RBRenameAndDeprecateClassTransformation >> className: aClassName newClassName: aNewClassName [

	className := aClassName.
	newClassName := aNewClassName
]

{ #category : 'printing' }
RBRenameAndDeprecateClassTransformation >> storeOn: aStream [

	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream
		nextPutAll: ' className ''';
		nextPutAll: className;
		nextPutAll: ''' newClassName: ''';
		nextPutAll: newClassName.
	aStream nextPut: $)
]

{ #category : 'private' }
RBRenameAndDeprecateClassTransformation >> tmpName [
	"return the hidden name of the subclass"

	^ (#TmpSubclass,className) asSymbol
]
