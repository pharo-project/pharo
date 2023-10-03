"
This tests the OSEnvironment. The tests assume that there is at least 1 environment variable given.
"
Class {
	#name : 'OSEnvironmentTest',
	#superclass : 'TestCase',
	#category : 'System-OSEnvironments-Tests',
	#package : 'System-OSEnvironments-Tests'
}

{ #category : 'helper' }
OSEnvironmentTest >> envNameForTest [

	^ 'PHAROTEST'
]

{ #category : 'helper' }
OSEnvironmentTest >> instance [
	^ Smalltalk os environment
]

{ #category : 'running' }
OSEnvironmentTest >> tearDown [

	self instance removeKey: self envNameForTest.
	super tearDown
]

{ #category : 'tests' }
OSEnvironmentTest >> testAsDictionary [
	self assert: self instance asDictionary isDictionary
]

{ #category : 'tests' }
OSEnvironmentTest >> testAssociations [
	| associations |
	associations := self instance associations.
	self denyEmpty: associations.
	self assert: associations anyOne key isString.
	self assert: associations anyOne value isString
]

{ #category : 'tests' }
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

{ #category : 'tests' }
OSEnvironmentTest >> testEnvironmentFor [

	| compareDictionary |

	compareDictionary := ((Smalltalk hasClassNamed: #UnixEnvironment) and: [Smalltalk hasClassNamed: #Win32Environment])
		ifTrue: [ "NativeBoost is present in the image"
			{ 	MacOSPlatform -> #UnixEnvironment.
	 		  	MacOSXPlatform -> #UnixEnvironment.
				UnixPlatform -> #UnixEnvironment.
				Win32Platform -> #Win32Environment  } asDictionary ]
		ifFalse: [ "NativeBoost is NOT present in the image"
			{ 	MacOSPlatform -> #NullOSEnvironment.
	 		  	MacOSXPlatform -> #NullOSEnvironment.
				UnixPlatform -> #NullOSEnvironment.
				Win32Platform -> #NullOSEnvironment  } asDictionary ].

	compareDictionary keysAndValuesDo: [ :platformClass :envClassName |
		self
			assert:  (OSEnvironment environmentFor: platformClass new) class name
			equals: envClassName ]
]

{ #category : 'tests' }
OSEnvironmentTest >> testKeys [
	| env keys |
	env := self instance.
	keys := env keys.

	self denyEmpty: keys.
	self assert: keys anyOne isString.
	self assert: (env includesKey: keys anyOne)
]

{ #category : 'tests' }
OSEnvironmentTest >> testSettingEnvValueDuringChangesValueInDuringBlock [

	| envName expected |
	envName := self envNameForTest.
	self instance setEnv: envName value: 'Before'.
	expected := 'After'.
	self instance setEnv: envName value: expected during: [
		| actual |
		actual := self instance at: envName.
		self assert: actual equals: expected ]
]

{ #category : 'tests' }
OSEnvironmentTest >> testSettingEnvValueDuringRevertsValueAfterDuringBlock [

	| actual envName expected |
	envName := self envNameForTest.
	self instance setEnv: envName value: 'Before'.
	expected := 'After'.
	self instance setEnv: envName value: expected during: [  ].
	actual := self instance at: envName.
	self assert: actual equals: expected
]

{ #category : 'tests' }
OSEnvironmentTest >> testSettingNewEnvValueDuringRemovesItAfterDuringBlock [

	| envName |
	envName := self envNameForTest.
	self instance setEnv: envName value: 'During' during: [  ].
	self deny: (self instance includesKey: envName)
]

{ #category : 'tests' }
OSEnvironmentTest >> testValues [
	| env values |
	env := self instance.
	values := env values.

	self denyEmpty: values.
	self assert: values anyOne isString.
	self assert: (env includes: values anyOne)
]
