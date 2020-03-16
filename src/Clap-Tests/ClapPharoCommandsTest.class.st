Class {
	#name : #ClapPharoCommandsTest,
	#superclass : #TestCase,
	#instVars : [
		'context'
	],
	#category : #'Clap-Tests-Commands'
}

{ #category : #testing }
ClapPharoCommandsTest class >> isAbstract [
	^ self = ClapPharoCommandsTest
]

{ #category : #testing }
ClapPharoCommandsTest class >> shouldInheritSelectors [
	^ true
]

{ #category : #asserting }
ClapPharoCommandsTest >> assertSuccess [
	self assert: context exitStatus equals: 0
]

{ #category : #'tests - fixture' }
ClapPharoCommandsTest >> lineEnding [
	^ OSPlatform current lineEnding
]

{ #category : #accessing }
ClapPharoCommandsTest >> outputString [
	^ context stdio stdout contents utf8Decoded
]
