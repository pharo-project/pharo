"
I'm a specific store for Windows file systems
"
Class {
	#name : #WindowsStore,
	#superclass : #DiskStore,
	#instVars : [
		'disk'
	],
	#classVars : [
		'Disks'
	],
	#category : #'FileSystem-Disk-Store'
}

{ #category : #accessing }
WindowsStore class >> delimiter [
	^ $\
]

{ #category : #accessing }
WindowsStore class >> isCaseSensitive [
	^ false
]

{ #category : #accessing }
WindowsStore class >> maxFileNameLength [
	self flag: 'TODO: more tests needed here!'.
	^ 255
]

{ #category : #accessing }
WindowsStore class >> separator [ 
	^ $\
]

{ #category : #public }
WindowsStore >> checkName: aFileName fixErrors: fixing [
	"Check if the file name contains any invalid characters"
	
	| fName badChars |
	fName := super checkName: aFileName fixErrors: fixing.
	badChars := (#( $: $< $> $| $/ $\ $? $* $"), ((0 to: 31) collect: [:n | n asCharacter])) asSet.
	
	(fName includesAnyOf: badChars)
		ifFalse: [^ fName].
	
	fixing ifFalse: [^self error: 'filename contains invalid characters'].
	
	^ fName collect:
		[:char | (badChars includes: char) 
				ifTrue: [$#] 
				ifFalse: [char]]
]

{ #category : #converting }
WindowsStore >> currentDisk [
	^ disk ifNil: [  disk := FileSystem workingDirectory path segments first ]
]

{ #category : #converting }
WindowsStore >> pathFromString: aString [
	"Need to distinguish '' and '/' , so tag latter with invalid character ':'  "
	| normalized pathClass pathElements |
	normalized := aString copy replaceAll: UnixStore delimiter with: self delimiter.
	pathElements := self delimiter split: normalized.
	pathClass := (Path isAbsoluteWindowsPath: normalized)
		ifTrue: [ (normalized = self delimiter asString) ifTrue: [ pathElements := { ':' } ].
			AbsolutePath ]
		ifFalse: [ self stripDrive: pathElements.
			RelativePath ].
	^pathClass withAll: pathElements
]

{ #category : #converting }
WindowsStore >> printPath: aPath on: aStream [
	| hasDrive |
	aPath isRoot
		ifTrue: [ ^ self ].	"effectively Windows root is empty string"
	aPath isWorkingDirectory
		ifTrue: [ ^ aPath printOn: aStream delimiter: self delimiter ].
	aPath isRelative
		ifTrue: [ ^ aPath printOn: aStream delimiter: self delimiter ].
	aPath segments first first = $:
		ifTrue: [ ^ aStream nextPut: self delimiter ].	"as tagged in #pathFromString:  "
	hasDrive := aPath segments first second = $:.
	(hasDrive not )
		ifTrue: [ aStream nextPut: self delimiter ].
	aPath printOn: aStream delimiter: self delimiter.
	(hasDrive and: [ aPath segments size = 1 ])
		ifTrue: [ aStream nextPut: self delimiter ]
]

{ #category : #converting }
WindowsStore >> stripDrive: pathElements [
	pathElements ifNotEmpty: [ pathElements at: 1 put: ( ($: split: pathElements first) last)  ]
]
