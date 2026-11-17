Class {
	#name : 'DeprecationTest',
	#superclass : 'TestCase',
	#category : 'Kernel-Extended-Tests-Deprecation',
	#package : 'Kernel-Extended-Tests',
	#tag : 'Deprecation'
}

{ #category : 'helpers' }
DeprecationTest >> callerIsTransformedWhenSending: aSelector [
	"Compile a caller of aSelector in ExampleForDeprecationTest, run it with transformations active, and answer whether it now sends #newMessage instead."

	| classFactory oldRaiseWarning oldActivateTransformations caller transformed |
	classFactory := ClassFactoryForTestCase environment: SystemEnvironment new.
	classFactory silentlyCompile: 'sendsDeprecatedMessage ^ self ' , aSelector in: ExampleForDeprecationTest.
	[
		oldRaiseWarning := Deprecation raiseWarning.
		oldActivateTransformations := Deprecation activateTransformations.
		Deprecation raiseWarning: false.
		Deprecation activateTransformations: true.
		ExampleForDeprecationTest new perform: #sendsDeprecatedMessage.
		caller := ExampleForDeprecationTest compiledMethodAt: #sendsDeprecatedMessage.
		transformed := (caller sendsSelector: #newMessage) and: [ (caller sendsSelector: aSelector) not ] ] ensure: [
			Deprecation raiseWarning: oldRaiseWarning.
			Deprecation activateTransformations: oldActivateTransformations.
			ExampleForDeprecationTest removeSelector: #sendsDeprecatedMessage ].
	^ transformed
]

{ #category : 'tests' }
DeprecationTest >> testDeprecationInCompatibilityPeriodDoesNotTransform [

	self deny: (self callerIsTransformedWhenSending: #deprecatedMessageWithTransformInCompatibilityPeriod)
]

{ #category : 'tests' }
DeprecationTest >> testDeprecationMajorVersion [

	self
		assert: (Deprecation new
			 version: #Pharo14;
			 deprecationMajorVersion)
		equals: 14.
	self
		assert: (Deprecation new
			 version: 'pharo13';
			 deprecationMajorVersion)
		equals: 13.
	self
		assert: (Deprecation new
			 version: #Pharo8;
			 deprecationMajorVersion)
		equals: 8.
	self assert: (Deprecation new
		 version: 'someversion';
		 deprecationMajorVersion) isNil.
	self assert: (Deprecation new
		 version: '';
		 deprecationMajorVersion) isNil.
	self assert: Deprecation new deprecationMajorVersion isNil
]

{ #category : 'tests' }
DeprecationTest >> testDeprecationWithoutVersionDoesNotTransform [

	self deny: (self callerIsTransformedWhenSending: #deprecatedMessageWithTransformAndNoVersion)
]

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

	self assert: (self callerIsTransformedWhenSending: #deprecatedMessageWithTransform)
]
