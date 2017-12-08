"
I am responsible for maintaining what version of Pharo and the VM is running.  I also track all of the update items that have been included in the image.

I'm invoked at auto start to get the latest plugins, etc.

Some queries are
	SystemVersion current major
	SystemVersion current minor
	SystemVersion current suffix
	SystemVersion current	highestUpdate
"
Class {
	#name : #SystemVersion,
	#superclass : #Object,
	#instVars : [
		'highestUpdate',
		'type',
		'major',
		'minor',
		'suffix',
		'build',
		'commitHash'
	],
	#classVars : [
		'Current'
	],
	#category : #'System-Support'
}

{ #category : #accessing }
SystemVersion class >> current [
	Current ifNil: [Current := SystemVersion new].
	^Current
]

{ #category : #deprecated }
SystemVersion class >> fromShortVersionString: aString [
	"Create a new SystemVersion from the given short version string. Useful for testing."

	| parts |	
	self 
		deprecated: 'Use #fromVersionString: instead'
		on: 	'22 September 2017' 
		in: 'Pharo-7.0+alpha.build.148.sha.368dcda1b1ba81215fb7355cfc1e901d45395bd8'.	
		
	parts := self parseShortVersionString: aString.
	^self new
		type: parts first;
		major: parts second;
		minor: parts third;
		suffix: parts fourth;
		yourself
]

{ #category : #'private - accessing' }
SystemVersion class >> invalidBuildNumber [
	"Return -1 to depict an invalid build number (allows to also accept 0 as build number)"
	
	^ -1

]

{ #category : #'instance creation' }
SystemVersion class >> major: major minor: minor [
	"Create a new instance of the receiver with the given major minor number.

		SystemVersion major: 6 minor: 1 
	"

	^self major: major minor: minor suffix: ''
]

{ #category : #'instance creation' }
SystemVersion class >> major: major minor: minor suffix: aString [ 
	"Create a new instance of the receiver with the given major minor number and suffix.

	 	SystemVersion major: 6 minor: 1 suffix: 'alpha'     
	"
	
	^self major: major minor: minor suffix: aString build: self invalidBuildNumber  
]

{ #category : #'instance creation' }
SystemVersion class >> major: major minor: minor suffix: aString build: build [ 
	"Create a new instance of the receiver with the given major minor number and suffix.

	 	SystemVersion major: 6 minor: 1 suffix: 'alpha' build: 148
	"
	
	^(self new)
		major: major;
		minor: minor;
		suffix: aString;
		build: build;
		yourself
]

{ #category : #'instance creation' }
SystemVersion class >> major: major minor: minor suffix: aString build: build commitHash: hashString [
	"Create a new instance of the receiver with the given major minor number and suffix.

	 	SystemVersion major: 6 minor: 1 suffix: 'alpha'     
	"
	
	^(self new)
		major: major;
		minor: minor;
		suffix: aString;
		build: build;
		commitHash: hashString;
		yourself
]

{ #category : #deprecated }
SystemVersion class >> newVersion: versionName [
	"Install a new SystemVersion as the current version. Only preserve the higest update recorded.
	This method is a candidate for being removed, I am not sure it ever makes sense to do this."
	
	| newVersion |
	
	self 
		deprecated: 'Method will be removed'
		on: 	'22 September 2017' 
		in: 'Pharo-7.0+alpha.build.148.sha.368dcda1b1ba81215fb7355cfc1e901d45395bd8'.
		
	newVersion := self fromShortVersionString: versionName.
	newVersion
		highestUpdate: self current highestUpdate.
	Current := newVersion

]

{ #category : #deprecated }
SystemVersion class >> parseShortVersionString: aString [ 
	"Answer the parts of aString, expected in short format <type><major>.<minor><suffix>."

	| stream length type major minor suffix |

	self 
		deprecated: 'Use #fromVersionString: instead'
		on: 	'22 September 2017' 
		in: 'Pharo-7.0+alpha.build.148.sha.368dcda1b1ba81215fb7355cfc1e901d45395bd8'.
	
	stream := aString readStream.
	[stream peek isDigit] whileFalse: [stream next].
	length := stream position.
	stream reset.
	type := (stream next: length).
	major := (stream upTo: $.) asInteger.
	stream := stream upToEnd readStream.
	[stream peek notNil and: [stream peek isDigit]] whileTrue: [stream next].
	length := stream position.
	stream reset.
	minor := (stream next: length) asInteger.
	suffix := stream upToEnd.
	^{type. major. minor. suffix}
]

{ #category : #deprecated }
SystemVersion class >> parseVersionString: versionString [ 
	"Answer the version of this release as version, date, update."
	"SystemVersion parseVersionString: 'Squeak3.1alpha of 28 February 2001 [latest update: #3966]' "
	| version date update |
	
	self 
		deprecated: 'Use #fromVersionString: instead'
		on: 	'22 September 2017' 
		in: 'Pharo-7.0+alpha.build.148.sha.368dcda1b1ba81215fb7355cfc1e901d45395bd8'.
	
	[ | stream | stream := versionString readStream.
	version := stream upToAll: ' of '.
	date := Date readFrom: stream.
	stream upToAll: ' #'.
	update := Number readFrom: stream ] 
		on: Error
		do: [ ^ nil ].
	^ {  version. date. update  }
]

{ #category : #'accessing - properties' }
SystemVersion >> build [

	 ^build
]

{ #category : #'accessing - properties' }
SystemVersion >> build: aBuildNumber [

	build := aBuildNumber 
]

{ #category : #private }
SystemVersion >> calculateHighestUpdate [ 
	|result|
	result := (major * 10000) + (minor * 1000).
	self hasValidBuildNumber ifTrue: [ result := result + build ].
	^result 
	

]

{ #category : #'accessing - properties' }
SystemVersion >> commitHash [
	"Answer the hash of a Git commit from which the image was bootstrapped, e.g. '9ce829e86f978a84fa68df20008a3c1ee52e1fae'"

	^ commitHash
]

{ #category : #'accessing - properties' }
SystemVersion >> commitHash: aString [

	"Set the hash of a Git commit from which the image was bootstrapped"

	commitHash := aString
]

{ #category : #accessing }
SystemVersion >> commitHashShort [
	"Answer the shorten hash of a Git commit from which the image was bootstrapped, e.g. '9ce829e"

	^ commitHash ifNotNil: [ commitHash first: (7 min: commitHash size) ]
]

{ #category : #'accessing - version strings' }
SystemVersion >> datedVersion [
	" E.g. 'Pharo2.0a of ''18 April 2012''' "

	^ self version asString , ' of ' , self date printString.
]

{ #category : #accessing }
SystemVersion >> dottedMajorMinor [
	
	^ self majorMinor: '.'.
]

{ #category : #testing }
SystemVersion >> hasMetadataIdentifiers [
	"Return true if we have metadata identifiers (see rule 10 on semver.org) that need to be separated with a + sign"
	
	^self suffix notEmpty | self hasValidBuildNumber | self commitHash notEmpty 
]

{ #category : #testing }
SystemVersion >> hasValidBuildNumber [
	"Return true if we have a valid build number"
	
	 ^self build ~= self class invalidBuildNumber
]

{ #category : #compatibility }
SystemVersion >> highestUpdate [
	 ^highestUpdate ifNil: [ highestUpdate := self calculateHighestUpdate ].

	

]

{ #category : #compatibility }
SystemVersion >> highestUpdate: anInteger [
	highestUpdate := anInteger
]

{ #category : #'accessing - version strings' }
SystemVersion >> imageVersionString [
	"Print the version compatibile with naming the image."
	
	^String streamContents: [:str |
		str nextPutAll: self type;
			 nextPut: $-;
			 nextPutAll: (self majorMinor: '.').
			
		self hasMetadataIdentifiers ifTrue: [ str nextPut: $+ ].		
		self suffix ifNotEmpty: [ str nextPutAll: self suffix ].
		self hasValidBuildNumber ifTrue: [ 
			self suffix ifNotEmpty: [ str nextPut: $.].
			str nextPutAll: 'build.'; nextPutAll: self build asString ].	
		self commitHash ifNotEmpty: [ str nextPutAll: '.sha.'; nextPutAll: self commitHash ].
		
		Smalltalk vm is32bit ifTrue: [ str nextPutAll: ' (32 Bit)' ]
								   ifFalse: [ str nextPutAll: ' (64 Bit)' ].					
	]
]

{ #category : #initialization }
SystemVersion >> initialize [
	super initialize.
	type := 'Pharo'.
	major := 0.
	minor := 0.	 	
	suffix := ''.
	build := self class invalidBuildNumber.
	commitHash := ''.
]

{ #category : #'accessing - properties' }
SystemVersion >> major [
	"Answer the major number of a version. 1 in '1.2'"

	^ major
]

{ #category : #'accessing - properties' }
SystemVersion >> major: anInteger [
	"Set the major part of the receiver"

	major := anInteger
]

{ #category : #utilities }
SystemVersion >> majorMinor: aSeparatingString [
	"Return the major/minor version number of the form XaSeparatingStringY, without any 'alpha' or 'beta' or other suffix."
	"SystemVersion current majorMinor: '-'"
	"SystemVersion current majorMinor: ''"

	^String streamContents: [:str |
		str 
			nextPutAll: self major printString; 
			nextPutAll: aSeparatingString;
			nextPutAll: self minor printString ]
]

{ #category : #accessing }
SystemVersion >> majorMinorVersion [
	
	^ self type, self dottedMajorMinor
]

{ #category : #'accessing - properties' }
SystemVersion >> minor [
	"Answer the minor number of a version. 2 in '1.2'"

	^ minor
]

{ #category : #'accessing - properties' }
SystemVersion >> minor: anInteger [
	"Set the minor part of the receiver"

	minor := anInteger
]

{ #category : #printing }
SystemVersion >> printOn: stream [
	stream nextPutAll: self imageVersionString
]

{ #category : #'accessing - version strings' }
SystemVersion >> shortVersionString [
	"Print a short version string."
	
	self type ifNil: [^'No version set'].
	^String streamContents: [:str |
		str 
			nextPutAll: self type;
			nextPutAll: self major printString;
			nextPut: $.;
			nextPutAll: self minor printString;
			nextPutAll: self suffix ]
]

{ #category : #'accessing - properties' }
SystemVersion >> suffix [
	"Answer the suffix (alpha, beta, rc  of a version. beta in '1.2beta'"

	^ suffix
]

{ #category : #'accessing - properties' }
SystemVersion >> suffix: anObject [
	"Set the value of suffix"

	suffix := anObject
]

{ #category : #'accessing - properties' }
SystemVersion >> type [
	"Answer the value of type"

	^ type
]

{ #category : #'accessing - properties' }
SystemVersion >> type: anObject [
	"Set the value of type"

	type := anObject
]

{ #category : #accessing }
SystemVersion >> version [
	"Answer the short version string. If the receiver needs upgrading, do it."
	
	^self shortVersionString
]
