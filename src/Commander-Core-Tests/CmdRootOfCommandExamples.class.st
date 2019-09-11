Class {
	#name : #CmdRootOfCommandExamples,
	#superclass : #CmdCommand,
	#instVars : [
		'savedParametersContext'
	],
	#category : #'Commander-Core-Tests'
}

{ #category : #testing }
CmdRootOfCommandExamples class >> isAbstract [
	^self = CmdRootOfCommandExamples 
]

{ #category : #execution }
CmdRootOfCommandExamples >> readParametersFromContext: aToolContext [
	savedParametersContext := aToolContext
]

{ #category : #accessing }
CmdRootOfCommandExamples >> savedParametersContext [
	^ savedParametersContext
]

{ #category : #accessing }
CmdRootOfCommandExamples >> savedParametersContext: anObject [
	savedParametersContext := anObject
]
