"
A GoferVersionReference refers to a specific version of a Monticello package in a particular repository. This class is the only one that can actually load the version, because it is the only one knowing where to find it.
"
Class {
	#name : #GoferResolvedReference,
	#superclass : #GoferVersionReference,
	#instVars : [
		'repository'
	],
	#category : 'Gofer-Core'
}

{ #category : #'instance creation' }
GoferResolvedReference class >> name: aString repository: aRepository [
	^ self basicNew initializeName: aString repository: aRepository
]

{ #category : #comparing }
GoferResolvedReference >> <= aResolvedReference [
	"Sort versions according to:

		1. package name
		2. branch name, list versions without branch last
		3. version number
		4. author name
		5. repository priority"
	
	self packageName = aResolvedReference packageName
		ifFalse: [ ^ self packageName <= aResolvedReference packageName ].
	self branch = aResolvedReference branch ifFalse: [ 
		^ (self branch isEmpty or: [ aResolvedReference branch isEmpty ])
			ifTrue: [ self branch size > aResolvedReference branch size ]
			ifFalse: [ self branch <= aResolvedReference branch ] ].
	self versionNumber = aResolvedReference versionNumber
		ifFalse: [ ^ self versionNumber <= aResolvedReference versionNumber ].
	self author = aResolvedReference author
		ifFalse: [ ^ self author <= aResolvedReference author ].
	self repository goferPriority = aResolvedReference repository goferPriority
		ifFalse: [ ^ self repository goferPriority <= aResolvedReference repository goferPriority ].
	^ true
]

{ #category : #initialization }
GoferResolvedReference >> initializeName: aString repository: aRepository [
	self initializeName: aString.
	repository := aRepository
]

{ #category : #actions }
GoferResolvedReference >> merge [
	"Merge-in the changes of this refence"
	Gofer new
		repository: self repository;
		package: package;
		version: self name;
		merge
]

{ #category : #accessing }
GoferResolvedReference >> repository [
	"Answer the repository of the receiver."
	
	^ repository
]

{ #category : #accessing }
GoferResolvedReference >> version [
	"Answer a Monticello version of the receiver."

	^ self repository versionFrom: self name
]
