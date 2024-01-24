"
I am a refactoring for rename class variables.

I rename the class variable in the class definition and in all methods refering to this variable.

My precondition verifies that the new variable is valid and not yet used in the whole class hierarchy.
"
Class {
	#name : 'ReRenameSharedVariableRefactoring',
	#superclass : 'RBVariableRefactoring',
	#instVars : [
		'newName'
	],
	#category : 'Refactoring-Core-Refactorings',
	#package : 'Refactoring-Core',
	#tag : 'Refactorings'
}

{ #category : 'instance creation' }
ReRenameSharedVariableRefactoring class >> model: aRBSmalltalk rename: aVarName to: aName in: aClass [
	^ self new
		model: aRBSmalltalk;
		rename: aVarName
			to: aName
			in: aClass;
		yourself
]

{ #category : 'instance creation' }
ReRenameSharedVariableRefactoring class >> rename: aVarName to: aName in: aClass [
	^ self new
		rename: aVarName
		to: aName
		in: aClass
]

{ #category : 'preconditions' }
ReRenameSharedVariableRefactoring >> applicabilityPreconditions [

	^ {
		  (ReIsNotAMetaclass class: class).
		  (ReIsValidSharedVariableName name: newName).
		  (ReIsVariableNotDefinedInHierarchy name: newName class: class).
		  (ReNameIsGlobalCondition new model: model className: newName) not.
		  "For now only rename a shared variable that is locally defined in a class."
		  (ReDirectlyDefinesSharedVariableCondition
			   classNamed: class name
			   inModel: self model
			   sharedVariables: { variableName }) }
]

{ #category : 'scripting api - conditions' }
ReRenameSharedVariableRefactoring >> checkPreconditions [ 

	self checkApplicabilityPreconditions 
]

{ #category : 'accessing' }
ReRenameSharedVariableRefactoring >> newName [
	^ newName
]

{ #category : 'accessing' }
ReRenameSharedVariableRefactoring >> newName: anObject [
	newName := anObject
]

{ #category : 'transforming' }
ReRenameSharedVariableRefactoring >> privateTransform [
	class
		renameClassVariable: variableName
		to: newName
		around: [self renameReferences]
]

{ #category : 'initialization' }
ReRenameSharedVariableRefactoring >> rename: aVarName to: aName in: aClass [
	self variable: aVarName class: aClass.
	newName := aName
]

{ #category : 'transforming' }
ReRenameSharedVariableRefactoring >> renameReferences [
	| replacer methods |
	replacer := self parseTreeRewriterClass
		rename: variableName
		to: newName
		handler:
			[ self refactoringError: ('<1s> is already defined as a method or block temporary <n> variable in this class or one of its subclasses' expandMacrosWith: newName) ].

	methods := class methodsUsingClassVariableNamed: variableName.
	methods do: [ :method | self convertMethod: method selector for: method modelClass using: replacer ]
]

{ #category : 'printing' }
ReRenameSharedVariableRefactoring >> storeOn: aStream [
	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream
		nextPutAll: ' rename: ''';
		nextPutAll: variableName;
		nextPutAll: ''' to: ''';
		nextPutAll: newName;
		nextPutAll: ''' in: '.
	class storeOn: aStream.
	aStream nextPut: $)
]
