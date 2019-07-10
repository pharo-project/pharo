"
I am a refactoring for rename class variables.

I rename the class variable in the class definition and in all methods refering to this variable.

My precondition verifies that the new variable is valid and not yet used in the whole class hierarchy.
"
Class {
	#name : #RBRenameClassVariableRefactoring,
	#superclass : #RBVariableRefactoring,
	#instVars : [
		'newName'
	],
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #'instance creation' }
RBRenameClassVariableRefactoring class >> model: aRBSmalltalk rename: aVarName to: aName in: aClass [ 
	^(self new)
		model: aRBSmalltalk;
		rename: aVarName
			to: aName
			in: aClass;
		yourself
]

{ #category : #'instance creation' }
RBRenameClassVariableRefactoring class >> rename: aVarName to: aName in: aClass [
	^self new
		rename: aVarName
		to: aName
		in: aClass
]

{ #category : #accessing }
RBRenameClassVariableRefactoring >> newName [
	^ newName
]

{ #category : #accessing }
RBRenameClassVariableRefactoring >> newName: anObject [
	newName := anObject
]

{ #category : #preconditions }
RBRenameClassVariableRefactoring >> preconditions [
	^(RBCondition isMetaclass: class) not 
		& (RBCondition isValidClassVarName: newName asString for: class) 
			& (RBCondition definesClassVariable: variableName asString in: class) 
			& (RBCondition hierarchyOf: class definesVariable: newName asString) not 
			& (RBCondition isGlobal: newName asString in: self model) not
]

{ #category : #initialization }
RBRenameClassVariableRefactoring >> rename: aVarName to: aName in: aClass [
	self variable: aVarName class: aClass.
	newName := aName
]

{ #category : #transforming }
RBRenameClassVariableRefactoring >> renameReferences [
	| replacer methods |
	replacer := RBParseTreeRewriter 
		rename: variableName
		to: newName
		handler: 
			[ self refactoringError: ('<1s> is already defined as a method or block temporary <n> variable in this class or one of its subclasses' expandMacrosWith: newName) ].

	methods := class methodsUsingClassVariableNamed: variableName.	
	methods do: [ :method | self convertMethod: method selector for: method modelClass using: replacer ]

]

{ #category : #printing }
RBRenameClassVariableRefactoring >> storeOn: aStream [ 
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

{ #category : #transforming }
RBRenameClassVariableRefactoring >> transform [
	class 
		renameClassVariable: variableName
		to: newName
		around: [self renameReferences]
]
