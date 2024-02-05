Class {
	#name : 'DeprecationTest',
	#superclass : 'TestCase',
	#category : 'Kernel-Tests-Exceptions',
	#package : 'Kernel-Tests',
	#tag : 'Exceptions'
}

{ #category : 'tests' }
DeprecationTest >> testTransformingDeprecation [

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
