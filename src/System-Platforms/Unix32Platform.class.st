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

	^ (Smalltalk vm operatingSystemName = 'unix')
		and: [ Smalltalk vm wordSize = 4
			and: [ Smalltalk vm architectureName ~= 'armv7l' ] ]
]

{ #category : #visiting }
Unix32Platform >> accept: aVisitor [

	^ aVisitor visitUnix32: self
]

{ #category : #testing }
Unix32Platform >> isUnix32 [
	^ true
]
