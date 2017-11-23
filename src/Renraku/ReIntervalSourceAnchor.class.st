"
This anchor stores the actual interval object which is set during initialization.
"
Class {
	#name : #ReIntervalSourceAnchor,
	#superclass : #ReSourceAnchor,
	#instVars : [
		'interval'
	],
	#category : #'Renraku-SourceAnchors'
}

{ #category : #'instance creation' }
ReIntervalSourceAnchor class >> entity: anEntity interval: anInterval [ 
	
	^ self basicNew
		initializeEnitity: anEntity
		interval: anInterval;
		yourself
]

{ #category : #'gt-inspector-extension' }
ReIntervalSourceAnchor >> gtInspectorAnchorIn: composite [
	<gtInspectorPresentationOrder: 0>

	(super gtInspectorAnchorIn: composite)
		initialize: [ :p | p selectionInterval: interval ]
]

{ #category : #initialization }
ReIntervalSourceAnchor >> initializeEnitity: anEntiy interval: anInterval [ 
	
	self initializeEnitity: anEntiy.
	interval := anInterval 
]

{ #category : #accessing }
ReIntervalSourceAnchor >> interval [

	^ interval 
]

{ #category : #testing }
ReIntervalSourceAnchor >> providesInterval [

	^ true
]
