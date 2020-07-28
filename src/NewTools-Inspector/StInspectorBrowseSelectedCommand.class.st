Class {
	#name : #StInspectorBrowseSelectedCommand,
	#superclass : #StInspectorBrowseCommand,
	#category : #'NewTools-Inspector-Command'
}

{ #category : #execution }
StInspectorBrowseSelectedCommand >> interestingObject [

	^ self context selectedObject
]
