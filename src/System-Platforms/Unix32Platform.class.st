"
urce: 22651600
"
Class {
	#name : #Unix32Platform,
	#superclass : #UnixPlatform,
	#category : #'System-Platforms-Unix'
}

{ #category : #testing }
Unix32Platform class >> isActivePlatform [
	^ (self currentPlatformName = 'unix') and: [ Smalltalk vm wordSize = 4 ]
]

{ #category : #testing }
Unix32Platform >> isUnix32 [
	^ true
]
