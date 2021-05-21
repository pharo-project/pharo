"
I am a command to extract selected ast node into temp variable using assignment. 

Internal Representation and Key Implementation Points.

    Instance Variables
	tempName:		<String>

"
Class {
	#name : #SycExtractTempCommand,
	#superclass : #SycSourceCodeRefactoringCommand,
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

{ #category : #execution }
SycExtractTempCommand >> asRefactorings [

	^ {RBExtractToTemporaryRefactoring
		  extract: sourceNode sourceInterval
		  to: tempName
		  from: method selector
		  in: method origin}
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
SycExtractTempCommand >> prepareFullExecutionInContext: aSourceCodeContext [
	super prepareFullExecutionInContext: aSourceCodeContext.
	
	tempName := UIManager default 
		request: 'New temp name' 
		initialAnswer: ''
		title: 'Extract code to temp variable'.
	tempName isEmptyOrNil ifTrue: [ CmdCommandAborted signal ]
]
