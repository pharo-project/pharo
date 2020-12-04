"
A model for the `thisContext` variable.
This is to be used when debugging.
"
Class {
	#name : #StInspectorThisContextNode,
	#superclass : #StInspectorSelfNode,
	#category : #'NewTools-Inspector-Model'
}

{ #category : #accessing }
StInspectorThisContextNode >> children [ 
	
	^ self value allInspectorNodes
]

{ #category : #accessing }
StInspectorThisContextNode >> key [
	^ 'thisContext'
]
