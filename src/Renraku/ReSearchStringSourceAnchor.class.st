"
This class stores a searchString which will be searched for in the entities source code on demand to find an interval of substring
"
Class {
	#name : #ReSearchStringSourceAnchor,
	#superclass : #ReSourceAnchor,
	#instVars : [
		'searchString'
	],
	#category : #'Renraku-SourceAnchors'
}

{ #category : #'instance creation' }
ReSearchStringSourceAnchor class >> entity: anEntity string: aString [ 
	
	^ self basicNew 
		initializeEnitity: anEntity
		searchString: aString;
		yourself
]

{ #category : #initialization }
ReSearchStringSourceAnchor >> initializeEnitity: anEntiy searchString: aString [ 
	
	self initializeEnitity: anEntiy.
	searchString := aString 
]

{ #category : #accessing }
ReSearchStringSourceAnchor >> interval [
	| start tweekedSubstring |
	
	tweekedSubstring := self searchString isSymbol
		ifTrue: [ self searchString keywords first ]
		ifFalse: [ self searchString ].
	
	start := (sourceEntity definition indexOfSubCollection: tweekedSubstring startingAt: 1).
	
	^ start to: start + tweekedSubstring size - 1
]

{ #category : #testing }
ReSearchStringSourceAnchor >> providesInterval [

	^ searchString notNil
]

{ #category : #accessing }
ReSearchStringSourceAnchor >> searchString [
	^ searchString
]
