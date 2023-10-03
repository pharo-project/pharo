"
I am a an object representing a Unix platform (64bits). Use myself to access platform specific features.

Please keep me polymorphic to the the other platform objects.

To get the current platform, you can evaluate:

OSPlatform current.
"
Class {
	#name : 'Unix64Platform',
	#superclass : 'UnixPlatform',
	#category : 'System-Platforms-Unix',
	#package : 'System-Platforms',
	#tag : 'Unix'
}

{ #category : 'testing' }
Unix64Platform class >> isActivePlatform [
	^ (Smalltalk vm operatingSystemName = 'unix') and: [ Smalltalk vm is64bit ]
]

{ #category : 'visiting' }
Unix64Platform >> accept: aVisitor [

	^ aVisitor visitUnix64: self
]

{ #category : 'testing' }
Unix64Platform >> isUnix64 [
	^ true
]
