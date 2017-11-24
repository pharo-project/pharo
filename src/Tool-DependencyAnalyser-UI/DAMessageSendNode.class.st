"
I'm a tree node representing a specific message send in the package under analysis.
"
Class {
	#name : #DAMessageSendNode,
	#superclass : #DAMethodDependencyNode,
	#category : #'Tool-DependencyAnalyser-UI-Nodes'
}

{ #category : #accessing }
DAMessageSendNode >> icon [
	^ self iconNamed: #smallLanguageIcon
]

{ #category : #displaying }
DAMessageSendNode >> label [
	^ String streamContents: [ :stream |
		stream
			nextPutAll: 'Sent in ';
			nextPutAll: self content methodClass asString;
			nextPutAll: '>>';
			nextPutAll: self content selector ]
]
