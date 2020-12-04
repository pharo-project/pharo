"
I model the `self` variable. This the value of this variable is always the host object. 
"
Class {
	#name : #StInspectorSelfNode,
	#superclass : #StInspectorNode,
	#category : #'NewTools-Inspector-Model'
}

{ #category : #accessing }
StInspectorSelfNode >> children [
 
	^ #()
]

{ #category : #accessing }
StInspectorSelfNode >> key [
	^ 'self'
]

{ #category : #accessing }
StInspectorSelfNode >> rawValue [

	^ self hostObject
]
