Class {
	#name : 'DeprecationTest',
	#superclass : 'TestCase',
	#category : 'Kernel-Extended-Tests',
	#package : 'Kernel-Extended-Tests'
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
DeprecationTest >> testNoInfiniteLoopWhenShowWarningIsTrue [
    "Regression test for https://github.com/pharo-project/pharo/issues/19396"
    
    | oldShowWarning oldActivateTransformations classFactory |
	  <ignoreNotImplementedSelectors: #(sendsDeprecatedMessageWithTransform)>
    classFactory := ClassFactoryForTestCase new.
    classFactory
        silentlyCompile: 'sendsDeprecatedMessageWithTransform ^ self deprecatedMessageWithTransform'
        in: ExampleForDeprecationTest.
    [
        oldShowWarning := Deprecation showWarning.
        oldActivateTransformations := Deprecation activateTransformations.
        Deprecation showWarning: true.
        Deprecation activateTransformations: true.
        ExampleForDeprecationTest new sendsDeprecatedMessageWithTransform
    ] ensure: [
        Deprecation showWarning: oldShowWarning.
        Deprecation activateTransformations: oldActivateTransformations ].
    ExampleForDeprecationTest
        removeSelector: #sendsDeprecatedMessageWithTransform
]

{ #category : 'tests' }
DeprecationTest >> testTransformingDeprecation [
	<ignoreNotImplementedSelectors: #(sendsDeprecatedMessageWithTransform)>
	| classFactory oldRaiseWarning oldActivateTransformations |
	classFactory := ClassFactoryForTestCase environment: SystemEnvironment new.
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
