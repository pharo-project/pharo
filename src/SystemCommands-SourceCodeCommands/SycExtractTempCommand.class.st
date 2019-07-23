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

{ #category : #accessing }
SycExtractTempCommand >> defaultMenuItemName [
	^ 'Extract temp'
]

{ #category : #execution }
SycExtractTempCommand >> execute [
	(RBInteractiveExtractTemp sourceNode: sourceNode method: method) executeExtractTemp
]
