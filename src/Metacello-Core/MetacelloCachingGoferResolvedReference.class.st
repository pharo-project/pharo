Class {
	#name : 'MetacelloCachingGoferResolvedReference',
	#superclass : 'GoferResolvedReference',
	#instVars : [
		'cachedVersion'
	],
	#category : 'Metacello-Core-Gofer',
	#package : 'Metacello-Core',
	#tag : 'Gofer'
}

{ #category : 'accessing' }
MetacelloCachingGoferResolvedReference >> version [
	"Answer a Monticello version of the receiver."

	^ cachedVersion ifNil: [ cachedVersion := super version ]
]

{ #category : 'querying' }
MetacelloCachingGoferResolvedReference >> workingCopy [
	"Answer a working copy or throw an error if not present."

	| pName |
	cachedVersion ifNil: [ ^ super workingCopy ].
	pName := cachedVersion package name.
	^ MCWorkingCopy allWorkingCopies
		  detect: [ :wc | pName = wc packageName ]
		  ifNone: [ self error: 'Working copy for ' , self name , ' not found' ]
]
