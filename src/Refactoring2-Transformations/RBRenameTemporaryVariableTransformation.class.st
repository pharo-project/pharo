"
RBRenameTemporaryVariableTransformation is responsible for renaming temporary variables, including arguments, inside a method.

Example: 
| transformation |
transformation := (RBRenameTemporaryVariableTransformation 
				rename: #rules to: #asdf
				in: #RBLintRuleTest
				selector: #openEditor)
				transform.
(ChangesBrowser changes: transformation model changes changes) open
	
Preconditions:
- Check whether this method has a temporary variable with the given name.
- Check whether the class does not have instance or class variables with the given name.
"
Class {
	#name : #RBRenameTemporaryVariableTransformation,
	#superclass : #RBMethodTransformation,
	#instVars : [
		'variableName',
		'newVariableName'
	],
	#category : #'Refactoring2-Transformations-Model'
}

{ #category : #api }
RBRenameTemporaryVariableTransformation class >> model: aRBModel rename: oldName to: newName in: aClass selector: aSelector [ 

	^ self new
		model: aRBModel;
		class: aClass
		selector: aSelector
		rename: oldName
		to: newName
]

{ #category : #api }
RBRenameTemporaryVariableTransformation class >> rename: oldName to: newName in: aClass selector: aSelector [ 

	^ self new
		class: aClass
		selector: aSelector
		rename: oldName
		to: newName
]

{ #category : #private }
RBRenameTemporaryVariableTransformation >> checkExistingVariable: variableNode [

	(variableNode isNil or: [ variableNode isVariable not ])
		ifTrue: [ self refactoringError: ('Temporary variable named {1} does not exist.'
							format: { variableName asString }) ].
	variableNode name = variableName
		ifFalse: [ self refactoringError: 'Selected interval should include variable declaration' ].
]

{ #category : #private }
RBRenameTemporaryVariableTransformation >> checkNewVariable: aParseTree [

	aParseTree
		ifNil: [ self refactoringError: ('Temporary variable named {1} is not defined by this method.'
							format: { variableName asString }) ].
	(aParseTree whoDefines: newVariableName)
		ifNotNil: [ self refactoringError: ('Temporary variable named {1} is already defined.'
							format: { newVariableName asString }) ].
	(aParseTree allDefinedVariables includes: newVariableName)
		ifTrue: [ self refactoringError: ('Temporary variable named {1} is already defined.'
							format: { newVariableName asString }) ].
]

{ #category : #api }
RBRenameTemporaryVariableTransformation >> class: aClass selector: aSelector rename: anOldSymbol to: aNewSymbol [ 

	self className: aClass.
	selector := aSelector.
	variableName := anOldSymbol.
	newVariableName := aNewSymbol
]

{ #category : #preconditions }
RBRenameTemporaryVariableTransformation >> preconditions [
	"check whether variable exists?
	 test instance variables as well"

	^ (RBCondition isValidInstanceVariableName: newVariableName for: self definingClass)
		& (RBCondition definesSelector: selector in: self definingClass)
		& (RBCondition definesInstanceVariable: newVariableName in: self definingClass) not
		& (RBCondition definesClassVariable: newVariableName in: self definingClass) not
]

{ #category : #executing }
RBRenameTemporaryVariableTransformation >> privateTransform [
	| parseTree definingNode variableNode |
	parseTree := self definingMethod.
	variableNode := parseTree variableWith: variableName.
	self checkExistingVariable: variableNode.
	definingNode := variableNode whoDefines: variableName.
	self checkNewVariable: definingNode.
	(self parseTreeRewriterClass rename: variableName to: newVariableName)
		executeTree: definingNode.
	class compileTree: parseTree
]

{ #category : #printing }
RBRenameTemporaryVariableTransformation >> storeOn: aStream [ 

	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream
		nextPutAll: ' renameTemporary: ';
		nextPutAll: variableName;
		nextPutAll: ' to: ''';
		nextPutAll: newVariableName;
		nextPutAll: ''' in: '.
	class storeOn: aStream.
	aStream
		nextPutAll: ' selector: #';
		nextPutAll: selector.
	aStream nextPut: $)
]
