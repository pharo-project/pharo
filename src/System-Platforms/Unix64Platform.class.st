"
I am a an object representing a Unix platform (64bits). Use myself to access platform specific features.

Please keep me polymorphic to the the other platform objects.

To get the current platform, you can evaluate:

OSPlatform current.
"
Class {
	#name : #Unix64Platform,
	#superclass : #UnixPlatform,
	#category : #'System-Platforms'
}

{ #category : #testing }
Unix64Platform class >> isActivePlatform [
	^ (self currentPlatformName = 'unix') and: [ Smalltalk vm wordSize = 8 ]
]

{ #category : #testing }
Unix64Platform >> isUnix64 [
	^ true
]
