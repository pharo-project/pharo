"
I'm a specific store for Unix file systems
"
Class {
	#name : #UnixStore,
	#superclass : #DiskStore,
	#category : #'FileSystem-Disk-Store'
}

{ #category : #public }
UnixStore class >> delimiter [
	^ $/
]

{ #category : #current }
UnixStore class >> isActiveClass [
	^ Smalltalk os isUnix and: [ super isActiveClass ]
]

{ #category : #public }
UnixStore class >> isCaseSensitive [
	"except for OSX the default is case sensitive"
	^ Smalltalk os isMacOSX not
]

{ #category : #public }
UnixStore class >> maxFileNameLength [

	^ 255
]

{ #category : #public }
UnixStore class >> separator [ 
	^ $:
]

{ #category : #public }
UnixStore >> checkName: aFileName fixErrors: fixing [
	"Check if the file name contains any invalid characters"
	| fName |
	fName := super checkName: aFileName fixErrors: fixing.
	
	(fName includes: self delimiter) ifFalse:
		[^fName].
	
	fixing ifFalse: [self error:'Invalid file name'].
	
	^ fName copyReplaceAll: self delimiter asString with: '#'
]
