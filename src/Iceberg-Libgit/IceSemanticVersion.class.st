"
I represent the semver (http://semver.org) of libgit2. 
Since I exist just to be able to compare versions, I'm limited (reason why I'm not promoted to a more general package).

"
Class {
	#name : #IceSemanticVersion,
	#superclass : #Magnitude,
	#instVars : [
		'major',
		'minor',
		'patch'
	],
	#category : #'Iceberg-Libgit-Core'
}

{ #category : #'instance creation' }
IceSemanticVersion class >> fromArray: anArray [
	^ self 
		major: anArray first 
		minor: anArray second 
		patch: anArray third
]

{ #category : #'instance creation' }
IceSemanticVersion class >> major: majorNumber minor: minorNumber patch: patchNumber [
	^ self basicNew 
		initializeMajor: majorNumber 
			minor: minorNumber 
			patch: patchNumber;
		yourself
]

{ #category : #'instance creation' }
IceSemanticVersion class >> new [
	self error: 'Use one of my siblings'
]

{ #category : #comparing }
IceSemanticVersion >> < aMagnitude [ 
	"Answer whether the receiver is less than the argument."
	| version |
	version := aMagnitude iceSemanticVersion.
	^ self asInteger < version asInteger

]

{ #category : #comparing }
IceSemanticVersion >> = aMagnitude [ 
	"Compare the receiver with the argument and answer with true if the 
	receiver is equal to the argument. Otherwise answer false."
	| version |
	
	version := aMagnitude iceSemanticVersion.
	^ self asInteger = version asInteger

]

{ #category : #converting }
IceSemanticVersion >> asInteger [
	"this does not works for versions 0.100+.*, but I do not think it will arrive to that"
	^ (self major * 10000) 
		+ (self minor * 100)
		+ self patch
]

{ #category : #comparing }
IceSemanticVersion >> hash [
	^ ((self species hash
		bitXor: self major)
		bitXor: self minor)
		bitXor: self patch

]

{ #category : #converting }
IceSemanticVersion >> iceSemanticVersion [
	^ self
]

{ #category : #initialization }
IceSemanticVersion >> initializeMajor: majorNumber minor: minorNumber patch: patchNumber [
	major := majorNumber.
	minor := minorNumber. 
	patch := patchNumber
]

{ #category : #accessing }
IceSemanticVersion >> major [
	^ major
]

{ #category : #accessing }
IceSemanticVersion >> minor [
	^ minor
]

{ #category : #accessing }
IceSemanticVersion >> patch [
	^ patch
]

{ #category : #printing }
IceSemanticVersion >> printOn: stream [ 
	stream 
		<< self major asString 
		<< '.' << self minor asString 
		<< '.' << self patch asString
]
