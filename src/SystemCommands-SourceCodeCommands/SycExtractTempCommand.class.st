"
I am a command to extract selected ast node into temp variable using assignment. 

Internal Representation and Key Implementation Points.

    Instance Variables
	tempName:		<String>

"
Class {
	#name : #SycExtractTempCommand,
	#superclass : #SycSourceCodeCommand,
	#instVars : [
		'tempName'
	],
	#category : #'SystemCommands-SourceCodeCommands'
}

{ #category : #execution }
SycExtractTempCommand class >> canBeExecutedInContext: aSourceCodeContext [
	super canBeExecutedInContext: aSourceCodeContext.
	
	^aSourceCodeContext isMethodSelected not 
]

{ #category : #execution }
SycExtractTempCommand >> applyResultInContext: aSourceCodeContext [
	super applyResultInContext: aSourceCodeContext.
	
	aSourceCodeContext showVariableNamed: tempName
]

{ #category : #accessing }
SycExtractTempCommand >> defaultMenuIconName [ 
	^ ##smallUpdateIcon
]

{ #category : #accessing }
SycExtractTempCommand >> defaultMenuItemName [
	^ 'Extract temp'
]

{ #category : #execution }
SycExtractTempCommand >> execute [
	| refactoring |
	refactoring := RBExtractToTemporaryRefactoring	
		extract: sourceNode sourceInterval
		to: tempName
		from: method selector
		in: method origin.
	
	self executeRefactoring: refactoring.
]

{ #category : #execution }
SycExtractTempCommand >> prepareFullExecutionInContext: aSourceCodeContext [
	super prepareFullExecutionInContext: aSourceCodeContext.
	
	tempName := UIManager default 
		request: 'New temp name' 
		initialAnswer: ''
		title: 'Extract code to temp variable'.
	tempName isEmptyOrNil ifTrue: [ CmdCommandAborted signal ]
]
