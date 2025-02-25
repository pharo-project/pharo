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
		'refactoringScopes'
	],
	#category : 'SystemCommands-SourceCodeCommands',
	#package : 'SystemCommands-SourceCodeCommands'
}

{ #category : 'testing' }
SycExtractTempCommand class >> canBeExecutedInContext: aSourceCodeContext [
		^ (super canBeExecutedInContext: aSourceCodeContext) and: [ aSourceCodeContext isMethodSelected not ]
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

	(ReExtractTempDriver new
		  extract: sourceNode sourceInterval
		  from: method selector
		  in: method origin;
		  scopes: refactoringScopes) runRefactoring
]

{ #category : 'testing' }
SycExtractTempCommand >> isComplexRefactoring [ 

	^ false
]

{ #category : 'execution' }
SycExtractTempCommand >> prepareFullExecutionInContext: aSourceCodeContext [
	super prepareFullExecutionInContext: aSourceCodeContext.
	refactoringScopes := aSourceCodeContext refactoringScopes
]
