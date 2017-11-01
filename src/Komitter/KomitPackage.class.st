"
I am a simple object used to cache the patch in order to save a bit of time
"
Class {
	#name : #KomitPackage,
	#superclass : #KomitObject,
	#instVars : [
		'package',
		'classes',
		'isFullyCommited',
		'dataRetriever',
		'patch'
	],
	#classInstVars : [
		'packages'
	],
	#category : 'Komitter-Models'
}

{ #category : #initialization }
KomitPackage class >> initialize [

	packages := Dictionary new
]

{ #category : #'instance creation' }
KomitPackage class >> package: package [

	^ self new
		package: package;
		yourself
]

{ #category : #removing }
KomitPackage class >> removePackage: package [

	(packages 
		removeKey: package package name
		ifAbsent: [ ^ self ]) flush
]

{ #category : #comparing }
KomitPackage >> <= aKomitObject [ 

	^ self package name <= aKomitObject package name
]

{ #category : #comparing }
KomitPackage >> = another [

	^ self species = another species and: [ self package = another package ]
]

{ #category : #accesing }
KomitPackage >> classNamed: aString [ 
	
	^ classes at: aString ifAbsentPut: [ KomitClass new trackedClass: aString  ]
]

{ #category : #protocol }
KomitPackage >> classes [

	classes ifNil: [
		classes := Dictionary new.
		self patch
			ifNil: [ #() ] 
			ifNotNil: [ :p |
				p operations 
					do: [ :each | | komitClass | 
						each definition className ifNotNil: [
							komitClass := (self classNamed: each definition className).
							each definition addOperation: each on: komitClass ] ] ].
	].
	^ classes values
]

{ #category : #protocol }
KomitPackage >> flush [
	super flush.
	
	classes ifNotNil: [ classes do: [ :each | each flush ] ].
	classes := nil.
	isFullyCommited := true
]

{ #category : #comparing }
KomitPackage >> hash [

	^ self package hash
]

{ #category : #initialize }
KomitPackage >> initialize [

	super initialize.

	expanded := true.
	isFullyCommited := true.
]

{ #category : #testing }
KomitPackage >> isEmpty [

	^ self package isEmpty
]

{ #category : #accessing }
KomitPackage >> isFullyCommited [
	^ isFullyCommited
]

{ #category : #accessing }
KomitPackage >> isFullyCommited: anObject [
	isFullyCommited := anObject
]

{ #category : #testing }
KomitPackage >> isPackage [

	^ true
]

{ #category : #'*Komitter-UI' }
KomitPackage >> koDestinationText [
	^ ''
]

{ #category : #'*Komitter-UI' }
KomitPackage >> koSourceText [
	^ 'No changes'
]

{ #category : #accessing }
KomitPackage >> package [
	^ package
]

{ #category : #accessing }
KomitPackage >> package: aPackage [

	package := aPackage
]

{ #category : #accessing }
KomitPackage >> patch [
	^ patch ifNil: [ patch := self package patch ]
]

{ #category : #printing }
KomitPackage >> printOn: aStream [

	super printOn: aStream.
	aStream << '[ '.
	self package name printOn: aStream.
	aStream << ' ]'.
]

{ #category : #accessing }
KomitPackage >> remotes [

	^ self package remotes
]
