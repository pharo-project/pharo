"
I am a platform to manage common features between linux and macOS since macOS is also unix.
"
Class {
	#name : 'AbstractUnixPlatform',
	#superclass : 'OSPlatform',
	#category : 'System-Platforms-Unix',
	#package : 'System-Platforms',
	#tag : 'Unix'
}

{ #category : 'testing' }
AbstractUnixPlatform class >> isAbstract [

	^ self = AbstractUnixPlatform
]
