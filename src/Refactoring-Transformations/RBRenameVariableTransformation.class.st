"
Renames a variable in a class and all its direct accesses. It is necessary to indicate whether the variable is an instance or class variable. It is not implemented as a composite transformation because the rename is made internally in the model.

Usage:
```
| transformation |
transformation := (RBRenameVariableTransformation
			rename: 'classBlock' to: 'asdf'
			in: #RBBasicLintRuleTestData
			classVariable: false)
			generateChanges.
(StRefactoringPreviewPresenter for: transformation) open
```

Preconditions:
overrides from RBAddVariableRefactoring and RBRemoveVariableRefactoring
"
Class {
	#name : 'RBRenameVariableTransformation',
	#superclass : 'RBVariableTransformation',
	#instVars : [
		'newVariableName'
	],
	#category : 'Refactoring-Transformations-Model-Unused',
	#package : 'Refactoring-Transformations',
	#tag : 'Model-Unused'
}

{ #category : 'api' }
RBRenameVariableTransformation class >> model: aRBModel rename: aVariableName to: aNewVariableName in: aClassName classVariable: aBoolean [

	^ self new
		model: aRBModel;
		rename: aVariableName
		to: aNewVariableName
		class: aClassName
		classVariable: aBoolean;
		yourself
]

{ #category : 'api' }
RBRenameVariableTransformation class >> model: aRBModel renameClassVariable: aVariableName to: aNewVariableName in: aClassName [

	^ self new
		model: aRBModel;
		rename: aVariableName
		to: aNewVariableName
		class: aClassName
		classVariable: true;
		yourself
]

{ #category : 'api' }
RBRenameVariableTransformation class >> model: aRBModel renameInstanceVariable: aVariableName to: aNewVariableName in: aClassName [

	^ self new
		model: aRBModel;
		rename: aVariableName
		to: aNewVariableName
		class: aClassName
		classVariable: false;
		yourself
]

{ #category : 'api' }
RBRenameVariableTransformation class >> rename: aVariableName to: aNewVariableName in: aClassName classVariable: aBoolean [

	^ self new
		rename: aVariableName
		to: aNewVariableName
		class: aClassName
		classVariable: aBoolean;
		yourself
]

{ #category : 'api' }
RBRenameVariableTransformation class >> renameClassVarible: aVariableName to: aNewVariableName in: aClassName [

	^ self new
		rename: aVariableName
		to: aNewVariableName
		class: aClassName
		classVariable: true;
		yourself
]

{ #category : 'api' }
RBRenameVariableTransformation class >> renameInstanceVarible: aVariableName to: aNewVariableName in: aClassName [

	^ self new
		rename: aVariableName
		to: aNewVariableName
		class: aClassName
		classVariable: false;
		yourself
]

{ #category : 'preconditions' }
RBRenameVariableTransformation >> applicabilityPreconditions [

	| conds |
	class := self model classObjectFor: className.

	conds := isClassVariable
		         ifTrue: [
			         {
				         (RBCondition isMetaclass: class) not.
				         (RBCondition
					          isValidClassVarName: newVariableName asString
					          for: class).
				         (RBCondition
					          definesClassVariable: variableName asString
					          in: class) } ]
		         ifFalse: [
			         {
				         (RBCondition
					          isValidInstanceVariableName: newVariableName
					          for: class).
				         (RBCondition
					          definesInstanceVariable: variableName
					          in: class) } ].
	^ conds , {
		(RBCondition hierarchyOf: class definesVariable: newVariableName)
				not.
		self isNotGlobal }
]

{ #category : 'preconditions' }
RBRenameVariableTransformation >> isNotGlobal [

	^ (RBCondition isGlobal: newVariableName in: self model) not
]

{ #category : 'executing' }
RBRenameVariableTransformation >> privateTransform [

	isClassVariable
		ifTrue: [ self definingClass instanceSide renameClassVariable: variableName
						to: newVariableName around: [ self renameReferences ] ]
		ifFalse: [ self definingClass renameInstanceVariable: variableName
						to: newVariableName around: [ self renameReferences ]  ]
]

{ #category : 'private' }
RBRenameVariableTransformation >> referencesFor: aClass [

	^ isClassVariable
		ifTrue: [ aClass whichSelectorsReferToClassVariable: variableName ]
		ifFalse: [ aClass whichSelectorsReferToInstanceVariable: variableName ]
]

{ #category : 'api' }
RBRenameVariableTransformation >> rename: aVariableName to: aNewVariableName class: aClassName classVariable: aBoolean [

	self className: aClassName.
	variableName := aVariableName.
	newVariableName := aNewVariableName.
	isClassVariable := aBoolean
]

{ #category : 'private' }
RBRenameVariableTransformation >> renameReferences [

	| replacer subclasses |
	replacer := self parseTreeRewriterClass
				rename: variableName
				to: newVariableName
				handler: [ self refactoringError: ('<1s> is already defined as a method
					or block temporary <n> variable in this class or one of its subclasses'
					expandMacrosWith: newVariableName )].
	subclasses := (self model classObjectFor: className) withAllSubclasses.
	isClassVariable ifTrue: [ subclasses addAll: self definingClass classSide withAllSubclasses ].

	self model
		convertClasses: subclasses
		select: [ :aClass | self referencesFor: aClass ]
		using: replacer
]

{ #category : 'storing' }
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
