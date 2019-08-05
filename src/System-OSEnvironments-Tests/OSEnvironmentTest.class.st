"
This tests the OSEnvironment. The tests assume that there is at least 1 environment variable given.
"
Class {
	#name : #OSEnvironmentTest,
	#superclass : #TestCase,
	#category : #'System-OSEnvironments-Tests'
}

{ #category : #helper }
OSEnvironmentTest >> instance [
	^ Smalltalk os environment
]

{ #category : #tests }
OSEnvironmentTest >> testAsDictionary [
	self assert: self instance asDictionary isDictionary
]

{ #category : #tests }
OSEnvironmentTest >> testAssociations [
	| associations |
	associations := self instance associations.
	self denyEmpty: associations.
	self assert: associations anyOne key isString.
	self assert: associations anyOne value isString
]

{ #category : #tests }
OSEnvironmentTest >> testAtPut [
	| key value env |
	
	env := self instance.
	value := 1000 atRandom asString.
	key := 'OSEnvironmentTest >> #testAtPut >> ', value.
	
	env at: key put: value.
	self assert: (env at: key) equals: value.
	
	env removeKey: key.
	self deny: (env includesKey: key)
]

{ #category : #tests }
OSEnvironmentTest >> testEnvironmentFor [
	
	| compareDictionary |
	
	compareDictionary := ((Smalltalk hasClassNamed: #UnixEnvironment) and: [Smalltalk hasClassNamed: #Win32Environment])
		ifTrue: [ "NativeBoost is present in the image"
			{ 	MacOSPlatform -> #UnixEnvironment. 
	 		  	MacOSXPlatform -> #UnixEnvironment. 
				UnixPlatform -> #UnixEnvironment. 
				Win32Platform -> #Win32Environment  } asDictionary ]
		ifFalse: [ "NativeBoost is NOT present in the image"
			{ 	MacOSPlatform -> #PlatformIndependentEnvironment. 
	 		  	MacOSXPlatform -> #PlatformIndependentEnvironment. 
				UnixPlatform -> #PlatformIndependentEnvironment. 
				Win32Platform -> #PlatformIndependentEnvironment  } asDictionary ].

	compareDictionary keysAndValuesDo: [ :platformClass :envClassName |
		self 
			assert:  (OSEnvironment environmentFor: platformClass new) class name
			equals: envClassName ] 
]

{ #category : #tests }
OSEnvironmentTest >> testKeys [
	| env keys |
	env := self instance.
	keys := env keys.

	self denyEmpty: keys.
	self assert: keys anyOne isString.
	self assert: (env includesKey: keys anyOne)
]

{ #category : #tests }
OSEnvironmentTest >> testValues [
	| env values |
	env := self instance.
	values := env values.

	self denyEmpty: values.
	self assert: values anyOne isString.
	self assert: (env includes: values anyOne)
]
