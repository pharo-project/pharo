"
I am an expert on Mac OS X filesystem conventions. I resolve origins according to these conventions.
"
Class {
	#name : #MacOSResolver,
	#superclass : #PlatformResolver,
	#category : #'FileSystem-Core-Base-Resolver'
}

{ #category : #accessing }
MacOSResolver class >> platformName [
	^  'Mac OS'
]

{ #category : #origins }
MacOSResolver >> cache [
	^ self library / 'Caches'
]

{ #category : #origins }
MacOSResolver >> home [
	^ self directoryFromEnvVariableNamed: 'HOME'
]

{ #category : #origins }
MacOSResolver >> library [
	^ self userLibrary
]

{ #category : #origins }
MacOSResolver >> preferences [ 
	^ self home / 'Library' / 'Preferences'
]

{ #category : #origins }
MacOSResolver >> systemApplicationSupport [
	<origin>
	^ self systemLibrary / 'Application Support'
]

{ #category : #origins }
MacOSResolver >> systemLibrary [
	<origin>
	^  FileSystem disk root / 'Library'
]

{ #category : #origins }
MacOSResolver >> temp [
	^ '/tmp' asFileReference
]

{ #category : #origins }
MacOSResolver >> userApplicationSupport [
	<origin>
	^self userLibrary / 'Application Support'
]

{ #category : #origins }
MacOSResolver >> userLibrary [
	<origin>
	^  self home / 'Library'
]
