"
I represent the Fuel version number.

I know how to parse and print the version number and how to compare it to another version.
"
Class {
	#name : #FLVersion,
	#superclass : #Object,
	#instVars : [
		'major',
		'minor',
		'patch'
	],
	#classVars : [
		'Current'
	],
	#category : #'Fuel-Core-Configuration'
}

{ #category : #accessing }
FLVersion class >> current [
	^ Current ifNil: [
		Current := self
			newWithMajor: 5
			minor: 0
			patch: 2 ]
]

{ #category : #'instance creation' }
FLVersion class >> newWithMajor: majorNumber minor: minorNumber patch: patchNumber [
	^ self basicNew
		initializeWithMajor: majorNumber minor: minorNumber patch: patchNumber;
		initialize;
		yourself
]

{ #category : #comparing }
FLVersion >> = aVersion [
	^ super = aVersion or: [
		self class = aVersion class and: [
			self major = aVersion major and: [
				self minor = aVersion minor and: [
					self patch = aVersion patch ] ] ] ]
]

{ #category : #initialization }
FLVersion >> initializeWithMajor: majorNumber minor: minorNumber patch: patchNumber [
	major := majorNumber.
	minor := minorNumber.
	patch := patchNumber
]

{ #category : #accessing }
FLVersion >> major [
	^ major
]

{ #category : #accessing }
FLVersion >> minor [
	^ minor
]

{ #category : #accessing }
FLVersion >> patch [
	^ patch
]

{ #category : #printing }
FLVersion >> printOn: aStream [
	aStream
		print: self major;
		nextPut: $.;
		print: self minor;
		nextPut: $.;
		print: self patch
]
