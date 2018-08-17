"
Renames a variable in a class and all its direct accesses. It is necessary to indicate whether the variable is an instance or class variable. It is not implemented as a composite transformation because the rename is made internally in the model.

Usage:
transformation := (RBRenameVariableTransformation
			rename: 'classBlock' to: 'asdf'
			in: #RBBasicLintRuleTest
			classVariable: false)
			transform.
(ChangesBrowser changes: transformation model changes changes) open

Preconditions:
overrides from RBAddVariableRefactoring and RBRemoveVariableRefactoring
"
Class {
	#name : #RBRenameVariableTransformation,
	#superclass : #RBVariableTransformation,
	#instVars : [
		'newVariableName'
	],
	#category : #'Refactoring2-Transformations-Model'
}

{ #category : #api }
RBRenameVariableTransformation class >> classVariable: aVariableName class: aClassName [

	self deprecated: 'use #rename:to:in:classVariable: instead'
]

{ #category : #api }
RBRenameVariableTransformation class >> instanceVariable: aVariableName class: aClassName [

	self deprecated: 'Use #rename:to:in:classVariable: instead'
]

{ #category : #api }
RBRenameVariableTransformation class >> model: aRBModel classVariable: aVariableName class: aClassName [

	self deprecated: 'Use #model:rename:to:in:classVariable: instead'
]

{ #category : #api }
RBRenameVariableTransformation class >> model: aRBModel instanceVariable: aVariableName class: aClassName [

	self deprecated: 'Use #model:rename:to:in:classVariable: instead'
]

{ #category : #api }
RBRenameVariableTransformation class >> model: aRBModel rename: aVariableName to: aNewVariableName in: aClassName classVariable: aBoolean [

	^ self new
		model: aRBModel;
		rename: aVariableName
		to: aNewVariableName
		class: aClassName
		classVariable: aBoolean;
		yourself
]

{ #category : #api }
RBRenameVariableTransformation class >> model: aRBModel variable: aVariableName  class: aClassName classVariable: aBoolean [

	self deprecated: 'Use #model:variable:to:class:classVariable: instead'
]

{ #category : #api }
RBRenameVariableTransformation class >> rename: aVariableName to: aNewVariableName in: aClassName classVariable: aBoolean [

	^ self new
		rename: aVariableName
		to: aNewVariableName
		class: aClassName
		classVariable: aBoolean;
		yourself
]

{ #category : #api }
RBRenameVariableTransformation class >> variable: aVariableName class: aClassName classVariable: aBoolean [

	self deprecated: 'Use #rename:to:in:classVariable: instead'
]

{ #category : #preconditions }
RBRenameVariableTransformation >> preconditions [

	class := self model classObjectFor: className.

	^ (isClassVariable
		ifTrue: [ (RBCondition isMetaclass: class) not 
					& (RBCondition isValidClassVarName: newVariableName asString for: class) 
					& (RBCondition definesClassVariable: variableName asString in: class) ]
		ifFalse: [ (RBCondition isValidInstanceVariableName: newVariableName for: class) 
					& (RBCondition definesInstanceVariable: variableName in: class) ])
		& (RBCondition hierarchyOf: class definesVariable: newVariableName) not 
		& (RBCondition isGlobal: newVariableName in: self model) not
]

{ #category : #executing }
RBRenameVariableTransformation >> privateTransform [

	isClassVariable
		ifTrue: [ self definingClass theNonMetaClass renameClassVariable: variableName
						to: newVariableName around: [ self renameReferences ] ]
		ifFalse: [ self definingClass renameInstanceVariable: variableName
						to: newVariableName around: [ self renameReferences ]  ]
]

{ #category : #private }
RBRenameVariableTransformation >> referencesFor: aClass [

	^ isClassVariable
		ifTrue: [ aClass whichSelectorsReferToClassVariable: variableName ]
		ifFalse: [ aClass whichSelectorsReferToInstanceVariable: variableName ]
	
]

{ #category : #api }
RBRenameVariableTransformation >> rename: aVariableName to: aNewVariableName class: aClassName classVariable: aBoolean [

	self className: aClassName.
	variableName := aVariableName.
	newVariableName := aNewVariableName.
	isClassVariable := aBoolean.
]

{ #category : #private }
RBRenameVariableTransformation >> renameReferences [

	| replacer subclasses |
	replacer := RBParseTreeRewriter 
				rename: variableName
				to: newVariableName
				handler: [ self refactoringError: ('<1s> is already defined as a method
					or block temporary <n> variable in this class or one of its subclasses' 
					expandMacrosWith: newVariableName )].
	subclasses := (self model classObjectFor: className) withAllSubclasses.
	isClassVariable ifTrue: [ subclasses addAll: self definingClass withAllSubclasses ].
	
	self model
		convertClasses: subclasses
		select: [ :aClass | self referencesFor: aClass ]
		using: replacer
]

{ #category : #printing }
RBRenameVariableTransformation >> storeOn: aStream [ 

	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream nextPutAll: ' rename: '.
	variableName storeOn: aStream.
	aStream nextPutAll: ' to: '.
	newVariableName storeOn: aStream.
	aStream nextPutAll: ' class: '.
	class storeOn: aStream.
	aStream nextPutAll: ' classVariable: '.
	isClassVariable storeOn: aStream.
	aStream nextPut: $)
]
