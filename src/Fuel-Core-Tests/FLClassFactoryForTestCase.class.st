"
I am a custom class factory used by the Fuel test suite. I was originally a copy of ClassFactoryForTestCase.

I'm useful when classes needs to be created during the execution of the test. This avoids polluting your unit tests with dummy and mock classes.
A typical usage of it is:

TestCase subclass: #YourTest
instanceVariableNames: 'classFactory'

YourTest>>setUp
classFactory := FLClassFactoryForTestCase new

YourTest>>tearDown
classFactory cleanUp.

YourTest>>testIsBehavior
| cls |
cls := classFactory newClass.
self assert: cls isBehavior
"
Class {
	#name : #FLClassFactoryForTestCase,
	#superclass : #ClassFactoryForTestCase,
	#category : #'Fuel-Core-Tests-Support-ClassFactory'
}

{ #category : #cleaning }
FLClassFactoryForTestCase >> delete: aBehavior [

	createdBehaviors remove: aBehavior ifAbsent: [  ].
	aBehavior isObsolete ifTrue: [ ^ self ].

	self class environment at: #ChangeSet ifPresent: [ :changeSet | aBehavior removeFromChanges ].

	aBehavior removeFromSystemUnlogged
]
