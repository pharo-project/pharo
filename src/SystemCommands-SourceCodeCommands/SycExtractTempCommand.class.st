"
I am a command to extract selected ast node into temp variable using assignment. 

Internal Representation and Key Implementation Points.

    Instance Variables
	tempName:		<String>

"
Class {
	#name : 'SycExtractTempCommand',
	#superclass : 'SycSourceCodeRefactoringCommand',
	#instVars : [
		'tempName',
		'toolContext'
	],
	#category : 'SystemCommands-SourceCodeCommands',
	#package : 'SystemCommands-SourceCodeCommands'
}

{ #category : 'testing' }
SycExtractTempCommand class >> canBeExecutedInContext: aSourceCodeContext [
		^ (super canBeExecutedInContext: aSourceCodeContext) and: [ aSourceCodeContext isMethodSelected not ]
]

{ #category : 'execution' }
SycExtractTempCommand >> applyResultInContext: aSourceCodeContext [
	super applyResultInContext: aSourceCodeContext.
	
	aSourceCodeContext showVariableNamed: tempName
]

{ #category : 'accessing' }
SycExtractTempCommand >> defaultMenuIconName [
	^ ##smallUpdate
]

{ #category : 'accessing' }
SycExtractTempCommand >> defaultMenuItemName [
	^ 'Extract temp'
]

{ #category : 'execution' }
SycExtractTempCommand >> execute [

	ReExtractToTemporaryDriver new
		extract: sourceNode sourceInterval
		to: tempName
		from: method selector
		in: method origin;
		scopes: toolContext refactoringScopes; runRefactoring
]

{ #category : 'testing' }
SycExtractTempCommand >> isComplexRefactoring [
	^ false
]

{ #category : 'execution' }
SycExtractTempCommand >> prepareFullExecutionInContext: aSourceCodeContext [
	super prepareFullExecutionInContext: aSourceCodeContext.

	tempName := self morphicUIManager
		request: 'New temp name'
		initialAnswer: ''
		title: 'Extract code to temp variable'.
	tempName isEmptyOrNil ifTrue: [ CmdCommandAborted signal ].
	
	toolContext := aSourceCodeContext
]
