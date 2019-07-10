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

{ #category : #visiting }
Unix32Platform >> accept: aVisitor [

	^ aVisitor visitUnix32: self
]

{ #category : #testing }
Unix32Platform >> isUnix32 [
	^ true
]
