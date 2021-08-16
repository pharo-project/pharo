Class {
	#name : #UnixARM32Platform,
	#superclass : #Unix32Platform,
	#category : #'System-Platforms-Unix'
}

{ #category : #testing }
UnixARM32Platform class >> isActivePlatform [

	^ (self currentPlatformName = 'unix') 
		and: [ Smalltalk vm wordSize = 4 
			and: [ (Smalltalk vm getSystemAttribute: 1003) = 'armv7l' ] ] 
]
