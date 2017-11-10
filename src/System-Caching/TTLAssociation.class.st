"
I am TTLAssociation. 
I am an Assiciation.

I maintain a timestamp that gets updated each time my value changes.

I decide myself when I am stale, using a cache defined default time to live.
"
Class {
	#name : #TTLAssociation,
	#superclass : #Association,
	#instVars : [
		'timestamp'
	],
	#category : #System-Caching
}

{ #category : #testing }
TTLAssociation >> isStale: timeToLive [
	^ self now - timestamp >= timeToLive
]

{ #category : #accessing }
TTLAssociation >> key: aKey value: anObject [ 
	super key: aKey value: anObject.
	timestamp := self now
]

{ #category : #accessing }
TTLAssociation >> now [
	^ Time totalSeconds
]

{ #category : #accessing }
TTLAssociation >> timestamp [
	^ timestamp
]

{ #category : #accessing }
TTLAssociation >> value: anObject [
	super value: anObject.
	timestamp := self now
]
