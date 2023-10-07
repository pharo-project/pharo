"
I am group of unclassified methods
"
Class {
	#name : 'ClyUnclassifiedMethodGroup',
	#superclass : 'ClyMethodGroup',
	#category : 'Calypso-SystemQueries-Domain',
	#package : 'Calypso-SystemQueries',
	#tag : 'Domain'
}

{ #category : 'accessing' }
ClyUnclassifiedMethodGroup class >> groupName [

	^ Protocol unclassified
]

{ #category : 'testing' }
ClyUnclassifiedMethodGroup class >> isBasedOnProtocol [
	^true
]

{ #category : 'accessing' }
ClyUnclassifiedMethodGroup class >> priority [
	^8
]

{ #category : 'instance creation' }
ClyUnclassifiedMethodGroup class >> withMethodsFrom: aMethodQuery [

	^self named: self groupName on: aMethodQuery
]
