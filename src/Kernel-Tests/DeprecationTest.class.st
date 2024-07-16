Class {
	#name : 'DeprecationTest',
	#superclass : 'TestCase',
	#category : 'Kernel-Tests-Exceptions',
	#package : 'Kernel-Tests',
	#tag : 'Exceptions'
}

{ #category : 'tests' }
DeprecationTest >> testMessageTextFromBlock [
	| notification |
	"messageText should not be nil even the call is from a block"
	notification := [ self deprecated: 'test']
			on: (self class environment at: #Deprecation)
			do: [ :d | d ].
	self assert: notification messageText notNil
]

{ #category : 'tests' }
DeprecationTest >> testTransformingDeprecation [
	<ignoreNotImplementedSelectors: #(sendsDeprecatedMessageWithTransform)>
	| classFactory oldRaiseWarning oldActivateTransformations |
	classFactory := ClassFactoryForTestCase new.
	classFactory 
		silentlyCompile: 'sendsDeprecatedMessageWithTransform ^ self deprecatedMessageWithTransform'
		in: ExampleForDeprecationTest.
		
	self assert: ((ExampleForDeprecationTest compiledMethodAt: #sendsDeprecatedMessageWithTransform) sendsSelector: #deprecatedMessageWithTransform).
	self deny: ((ExampleForDeprecationTest compiledMethodAt: #sendsDeprecatedMessageWithTransform) sendsSelector: #newMessage).
	
	[ 
		oldRaiseWarning := Deprecation raiseWarning.
		oldActivateTransformations := Deprecation activateTransformations.
		Deprecation raiseWarning: false.
		Deprecation activateTransformations: true.
		ExampleForDeprecationTest new sendsDeprecatedMessageWithTransform
	] ensure: [
			Deprecation raiseWarning: oldRaiseWarning.
			Deprecation activateTransformations: oldActivateTransformations ].
	
	self deny: ((ExampleForDeprecationTest compiledMethodAt: #sendsDeprecatedMessageWithTransform) sendsSelector: #deprecatedMessageWithTransform).
	self assert: ((ExampleForDeprecationTest compiledMethodAt: #sendsDeprecatedMessageWithTransform) sendsSelector: #newMessage).

	ExampleForDeprecationTest 
		removeSelector: #sendsDeprecatedMessageWithTransform
]
