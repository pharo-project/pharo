"
I represent the version of the pharo image to build.
I contain

 - major version (mandatory)
 - minor version (optional, 0 if absent)
 - patch version (optional, 0 if absent)
 - suffix  (optional, nil if absent)
 - commit hash (optional, 'UNKNOWN_COMMIT' if absent)
 - build number (optional, 'UNKNOWN_BUILD' if absent)

I am used during the bootstrap process to parse this information and include it in the generated binary image.
"
Class {
	#name : #PBVersionInfo,
	#superclass : #Object,
	#instVars : [
		'repository',
		'commitHash',
		'buildNumber',
		'major',
		'minor',
		'patch',
		'suffix'
	],
	#category : 'Pharo30Bootstrap'
}

{ #category : #'instance-creation' }
PBVersionInfo class >> fromLongGitDescribe: aString [ 
	
	| parts commitish semanticVersion |
	parts := aString substrings: '-'.
	commitish := parts last allButFirst.
	semanticVersion := (parts allButLast: 2) joinUsing: '-'.
	
	^ self basicNew
		fromSemanticVersion: semanticVersion;
		commitHash: commitish;
		yourself 
]

{ #category : #'instance-creation' }
PBVersionInfo class >> fromSemanticVersion: aString [ 
	
	^ self basicNew
		fromSemanticVersion: aString;
		yourself
]

{ #category : #'instance creation' }
PBVersionInfo class >> invalidMajorVersion: anObject [

	self error: 'Invalid major version: ', anObject printString
]

{ #category : #'instance-creation' }
PBVersionInfo class >> major: anInteger [ 
	
	^ self basicNew
		major: anInteger;
		initialize
]

{ #category : #'instance creation' }
PBVersionInfo class >> new [

	self invalidMajorVersion: nil
]

{ #category : #accessing }
PBVersionInfo >> buildNumber [
	^ buildNumber ifNil: [ -1 ]
]

{ #category : #accessing }
PBVersionInfo >> buildNumber: anObject [
	buildNumber := anObject
]

{ #category : #accessing }
PBVersionInfo >> commitHash [
	^ commitHash ifNil: [ 'UNKNOWN_COMMIT' ]
]

{ #category : #accessing }
PBVersionInfo >> commitHash: aString [ 
	commitHash := aString
]

{ #category : #configuration }
PBVersionInfo >> fromSemanticVersion: aString [ 
	
	| version versionAndSuffix partsWithoutSuffix |
	version := aString.
	(version beginsWith: 'v')
		ifTrue: [ version := version allButFirst ].
	
	versionAndSuffix := version substrings: '-'.
	partsWithoutSuffix := versionAndSuffix first substrings: '.'.
	
	partsWithoutSuffix size < 1
		ifTrue: [ self invalidMajorVersion: version ].
	self major: partsWithoutSuffix first.
	
	partsWithoutSuffix size > 1
		ifTrue: [ self minor: partsWithoutSuffix second ].
	
	partsWithoutSuffix size > 2
		ifTrue: [ self patch: partsWithoutSuffix third ].
	
	versionAndSuffix size > 1
		ifTrue: [ self suffix: versionAndSuffix second ].
	
	^ self
]

{ #category : #validating }
PBVersionInfo >> invalidMajorVersion: anObject [
	
	^ self class invalidMajorVersion: anObject
]

{ #category : #accessing }
PBVersionInfo >> major [
	^ major
]

{ #category : #accessing }
PBVersionInfo >> major: anObject [

	self validateMajorVersion: anObject.
	major := anObject asInteger
]

{ #category : #accessing }
PBVersionInfo >> majorVersion [
	^ major 
]

{ #category : #accessing }
PBVersionInfo >> minor [
	^ minor ifNil: [ 0 ]
]

{ #category : #accessing }
PBVersionInfo >> minor: anObject [
	minor := anObject asInteger
]

{ #category : #accessing }
PBVersionInfo >> minorVersion [
	^ minor
]

{ #category : #accessing }
PBVersionInfo >> patch [
	^ patch ifNil: [ 0 ]
]

{ #category : #accessing }
PBVersionInfo >> patch: anObject [
	
	patch := anObject asInteger
]

{ #category : #accessing }
PBVersionInfo >> repository [
	^ repository
]

{ #category : #accessing }
PBVersionInfo >> repository: anObject [
	repository := anObject
]

{ #category : #accessing }
PBVersionInfo >> suffix [
	
	^ suffix
]

{ #category : #accessing }
PBVersionInfo >> suffix: aString [ 
	suffix := aString
]

{ #category : #validating }
PBVersionInfo >> validateMajorVersion: anObject [
	
	| majorNumber |
	majorNumber := anObject asInteger.
	majorNumber ifNil: [ self invalidMajorVersion: anObject ].
]
