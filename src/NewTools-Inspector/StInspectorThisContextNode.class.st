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
