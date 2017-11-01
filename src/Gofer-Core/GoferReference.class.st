"
A GoferReference is an abstract superclass for various kinds of references to Monticello packages and versions.
"
Class {
	#name : #GoferReference,
	#superclass : #Object,
	#instVars : [
		'name'
	],
	#category : 'Gofer-Core'
}

{ #category : #'instance creation' }
GoferReference class >> name: aString [
	^ self basicNew initializeName: aString
]

{ #category : #'instance creation' }
GoferReference class >> new [
	self error: 'Use #name: to initialize the receiver.'
]

{ #category : #comparing }
GoferReference >> = aReference [
	^ self class = aReference class and: [ self name = aReference name ]
]

{ #category : #comparing }
GoferReference >> hash [
	^ self name hash
]

{ #category : #initialization }
GoferReference >> initializeName: aString [
	name := aString
]

{ #category : #testing }
GoferReference >> isConfigurationReference [
	^ false
]

{ #category : #private }
GoferReference >> matches: aResolvedReference [
	"Answer true if the receiver matches aResolvedReference."

	self subclassResponsibility
]

{ #category : #accessing }
GoferReference >> name [
	"Answer the name of this reference."
	
	^ name
]

{ #category : #accessing }
GoferReference >> packageName [
	"Answer the package name."
	
	self subclassResponsibility
]

{ #category : #printing }
GoferReference >> printOn: aStream [
	super printOn: aStream.
	aStream nextPutAll: ' name: '; print: self name
]

{ #category : #querying }
GoferReference >> resolveAllWith: aGofer [
	"Answer a sorted collection of all resolved references within aGofer."

	^ aGofer allResolved select: [ :each | self matches: each ]
]

{ #category : #querying }
GoferReference >> resolveWith: aGofer [
	"Answer a single resolved reference with aGofer configuration, throw an error if the version can't be found.'"

	| references |
	references := self resolveAllWith: aGofer.
	^ references isEmpty
		ifTrue: [ self error: 'Unable to resolve ' , self name ]
		ifFalse: [ references last ]
]

{ #category : #querying }
GoferReference >> workingCopy [
	"Answer a working copy or throw an error if not present."

	^ self workingCopyIfAbsent: [ self error: 'Working copy for ' , self name , ' not found' ]
]

{ #category : #querying }
GoferReference >> workingCopyIfAbsent: aBlock [
	"Answer a working copy or throw an error if not present."

	^ MCWorkingCopy allManagers
		detect: [ :each | self packageName = each packageName ]
		ifNone: aBlock
]
