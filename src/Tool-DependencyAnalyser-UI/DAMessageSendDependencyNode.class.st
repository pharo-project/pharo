"
I'm a tree node representing a specific message send in the package under analysis.
"
Class {
	#name : #DAMessageSendDependencyNode,
	#superclass : #DAMessageSendNode,
	#instVars : [
		'sentMessages'
	],
	#category : #Tool-DependencyAnalyser-UI-Nodes
}

{ #category : #displaying }
DAMessageSendDependencyNode >> label [
	^ String streamContents: [ :stream |
		stream
			nextPutAll: self content methodClass asString;
			nextPutAll: '>>';
			nextPutAll: self content selector;
			nextPutAll: ' sends #'.
		sentMessages do: [ :message |
			stream nextPutAll: message ] ]
]

{ #category : #setting }
DAMessageSendDependencyNode >> sentMessages: aListOfSelectors [ 
	sentMessages := aListOfSelectors
]
