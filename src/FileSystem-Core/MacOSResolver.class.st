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
MacOSResolver >> desktop [
	^ self home / 'Desktop'
]

{ #category : #origins }
MacOSResolver >> documents [
	^ self home / 'Documents'
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

{ #category : #resolving }
MacOSResolver >> supportedOrigins [
	^ super supportedOrigins , #(userApplicationSupport systemApplicationSupport systemLibrary userLibrary)
]

{ #category : #origins }
MacOSResolver >> systemApplicationSupport [
	^ self systemLibrary / 'Application Support'
]

{ #category : #origins }
MacOSResolver >> systemLibrary [
	^  FileSystem disk root / 'Library'
]

{ #category : #origins }
MacOSResolver >> temp [
	^ '/tmp' asFileReference
]

{ #category : #origins }
MacOSResolver >> userApplicationSupport [
	^self userLibrary / 'Application Support'
]

{ #category : #origins }
MacOSResolver >> userLibrary [
	^  self home / 'Library'
]
