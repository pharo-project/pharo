Class {
	#name : #MetacelloCachingGoferResolvedReference,
	#superclass : #GoferResolvedReference,
	#instVars : [
		'cachedVersion'
	],
	#category : #'Metacello-MC-Gofer'
}

{ #category : #accessing }
MetacelloCachingGoferResolvedReference >> version [
	"Answer a Monticello version of the receiver."

	cachedVersion == nil ifTrue: [ cachedVersion := super version ].
	^cachedVersion
]

{ #category : #querying }
MetacelloCachingGoferResolvedReference >> workingCopy [
	"Answer a working copy or throw an error if not present."

	| pName |
	cachedVersion == nil ifTrue: [ ^ super workingCopy ].
	pName := cachedVersion package name.
	^ MCWorkingCopy allWorkingCopies
		  detect: [ :wc | pName = wc packageName ]
		  ifNone: [ self error: 'Working copy for ' , self name , ' not found' ]
]
