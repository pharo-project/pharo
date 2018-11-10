Class {
	#name : #CmdRootOfCommandExamples,
	#superclass : #CmdCommand,
	#instVars : [
		'savedParametersContext'
	],
	#category : #'Commander-Core-Tests'
}

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
