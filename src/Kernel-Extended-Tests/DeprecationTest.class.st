Class {
	#name : 'DeprecationTest',
	#superclass : 'TestCase',
	#category : 'Kernel-Extended-Tests-Deprecation',
	#package : 'Kernel-Extended-Tests',
	#tag : 'Deprecation'
}

{ #category : 'helpers' }
DeprecationTest >> callerIsTransformedWhenSending: aSelector [

	^ self callerIsTransformedWhenSending: aSelector rewriting: CompatibilityPeriodRewriteDeprecation
]

{ #category : 'helpers' }
DeprecationTest >> callerIsTransformedWhenSending: aSelector rewriting: aRewriting [
	"Compile a caller of aSelector in ExampleForDeprecationTest, run it with aRewriting, and answer whether it now sends #newMessage instead. The deprecation is handled here and its default action run explicitly, since on the Pharo CI SUnit makes a deprecation reaching it an error."

	| classFactory caller |
	classFactory := ClassFactoryForTestCase environment: SystemEnvironment new.
	classFactory silentlyCompile: 'sendsDeprecatedMessage ^ self ' , aSelector in: ExampleForDeprecationTest.
	^ [
		  self
			  reaction: IgnoreDeprecationReaction
			  rewriting: aRewriting
			  during: [ self runDefaultActionOfDeprecationsIn: [ ExampleForDeprecationTest new perform: #sendsDeprecatedMessage ] ].
		  caller := ExampleForDeprecationTest compiledMethodAt: #sendsDeprecatedMessage.
		  (caller sendsSelector: #newMessage) and: [ (caller sendsSelector: aSelector) not ] ] ensure: [
		  ExampleForDeprecationTest removeSelector: #sendsDeprecatedMessage ]
]

{ #category : 'tests - compatibility period' }
DeprecationTest >> deprecationIn: aMethod [

	^ Deprecation new
		  context: (Context
			   sender: nil
			   receiver: nil
			   method: aMethod
			   arguments: #(  ));
		  yourself
]

{ #category : 'helpers' }
DeprecationTest >> reaction: aReaction rewriting: aRewriting during: aBlock [
	"Run aBlock with these deprecation strategies, and restore the ones of the image afterwards."

	| oldReaction oldRewriting |
	oldReaction := Deprecation reaction.
	oldRewriting := Deprecation rewriting.
	Deprecation
		reaction: aReaction;
		rewriting: aRewriting.
	^ aBlock ensure: [
		  Deprecation
			  reaction: oldReaction;
			  rewriting: oldRewriting ]
]

{ #category : 'helpers' }
DeprecationTest >> runDefaultActionOfDeprecationsIn: aBlock [
	"Run the default action of the deprecations signaled by aBlock, as if no one handled them: on the Pharo CI, SUnit makes a deprecation reaching it an error."

	^ aBlock
		  on: Deprecation
		  do: [ :deprecation | deprecation resume: deprecation defaultAction ]
]

{ #category : 'tests - strategies' }
DeprecationTest >> testAlwaysRewritingTransformsWithoutVersion [

	self assert:
		(self callerIsTransformedWhenSending: #deprecatedMessageWithTransformAndNoVersion rewriting: AlwaysRewriteDeprecation)
]

{ #category : 'tests - strategies' }
DeprecationTest >> testCompatibilityPeriodRewritingNeedsAVersion [

	self deny:
		(self
			 callerIsTransformedWhenSending: #deprecatedMessageWithTransformAndNoVersion
			 rewriting: CompatibilityPeriodRewriteDeprecation)
]

{ #category : 'tests - compatibility period' }
DeprecationTest >> testDeprecatedPackageNames [

	| deprecation |
	deprecation := Deprecation new.
	self assert: (deprecation isDeprecatedPackageName: 'Deprecated14').
	self assert: (deprecation isDeprecatedPackageName: 'Deprecated14-Kernel').
	self deny: (deprecation isDeprecatedPackageName: 'Morphic-Deprecated').
	self deny: (deprecation isDeprecatedPackageName: 'Deprecation').
	self deny: (deprecation isDeprecatedPackageName: 'DeprecatedSomething')
]

{ #category : 'tests - strategies' }
DeprecationTest >> testDeprecatedSettingsChooseStrategies [

	self reaction: LogDeprecationReaction rewriting: CompatibilityPeriodRewriteDeprecation during: [
		[
			Deprecation activateTransformations: false.
			self assert: Deprecation rewriting identicalTo: NeverRewriteDeprecation.
			Deprecation activateTransformations: true.
			self assert: Deprecation rewriting identicalTo: CompatibilityPeriodRewriteDeprecation.
			Deprecation raiseWarning: true.
			self assert: Deprecation reaction identicalTo: WarnDeprecationReaction.
			Deprecation raiseWarning: false.
			self assert: Deprecation reaction identicalTo: LogDeprecationReaction.
			Deprecation showWarning: false.
			self assert: Deprecation reaction identicalTo: IgnoreDeprecationReaction ]
			on: Deprecation
			do: [ :deprecation | deprecation resume ] ]
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
DeprecationTest >> testDeprecationWithVersionAndNoDateIsDeprecated [

	self assert: (ExampleForDeprecationTest >> #deprecatedMessageWithTransformAndNoDate) isDeprecated
]

{ #category : 'tests' }
DeprecationTest >> testDeprecationWithVersionAndNoDateTransforms [

	self assert: (self callerIsTransformedWhenSending: #deprecatedMessageWithTransformAndNoDate)
]

{ #category : 'tests' }
DeprecationTest >> testDeprecationWithoutVersionDoesNotTransform [

	self deny: (self callerIsTransformedWhenSending: #deprecatedMessageWithTransformAndNoVersion)
]

{ #category : 'tests - strategies' }
DeprecationTest >> testErrorAtEndOfPeriodReactionLogsInThePeriod [

	| notified |
	notified := false.
	self reaction: ErrorAtEndOfCompatibilityPeriodDeprecationReaction rewriting: NeverRewriteDeprecation during: [
		[ self runDefaultActionOfDeprecationsIn: [ ExampleForDeprecationTest new deprecatedMessageWithTransformInCompatibilityPeriod ] ]
			on: DeprecationPerformedNotification
			do: [ :notification |
				notified := true.
				notification resume ] ].
	self assert: notified
]

{ #category : 'tests - strategies' }
DeprecationTest >> testErrorAtEndOfPeriodReactionLogsWithoutVersion [

	| notified |
	notified := false.
	self reaction: ErrorAtEndOfCompatibilityPeriodDeprecationReaction rewriting: NeverRewriteDeprecation during: [
		[ self runDefaultActionOfDeprecationsIn: [ ExampleForDeprecationTest new deprecatedMessageWithTransformAndNoVersion ] ]
			on: DeprecationPerformedNotification
			do: [ :notification |
				notified := true.
				notification resume ] ].
	self assert: notified
]

{ #category : 'tests - strategies' }
DeprecationTest >> testErrorAtEndOfPeriodReactionSignalsAnErrorAtTheEnd [

	self reaction: ErrorAtEndOfCompatibilityPeriodDeprecationReaction rewriting: NeverRewriteDeprecation during: [
		self
			should: [ self runDefaultActionOfDeprecationsIn: [ ExampleForDeprecationTest new deprecatedMessageWithTransform ] ]
			raise: Error ]
]

{ #category : 'tests - strategies' }
DeprecationTest >> testErrorReactionSignalsAnError [

	self reaction: ErrorDeprecationReaction rewriting: NeverRewriteDeprecation during: [
		self
			should: [ self runDefaultActionOfDeprecationsIn: [ ExampleForDeprecationTest new deprecatedMessageWithTransform ] ]
			raise: Error ]
]

{ #category : 'tests - strategies' }
DeprecationTest >> testEveryReactionRecordsTheDeprecation [
	"The warning reaction opens a blocking dialog, so it is left out."

	(DeprecationReaction allSubclasses copyWithout: WarnDeprecationReaction) do: [ :reaction |
		| seen |
		self reaction: reaction rewriting: NeverRewriteDeprecation during: [
			seen := Deprecation deprecationsWhile: [
				        [ self runDefaultActionOfDeprecationsIn: [ ExampleForDeprecationTest new deprecatedMessageWithTransform ] ]
					        on: Error , DeprecationPerformedNotification
					        do: [ :exception | exception return ] ] ].
		self assert: seen size = 1 description: reaction label ]
]

{ #category : 'tests - strategies' }
DeprecationTest >> testIgnoreReactionNotifiesNothing [

	| notified |
	notified := false.
	self reaction: IgnoreDeprecationReaction rewriting: NeverRewriteDeprecation during: [
		[ self runDefaultActionOfDeprecationsIn: [ ExampleForDeprecationTest new deprecatedMessageWithTransform ] ]
			on: DeprecationPerformedNotification
			do: [ :notification |
				notified := true.
				notification resume ] ].
	self deny: notified
]

{ #category : 'tests - compatibility period' }
DeprecationTest >> testKernelIsDecidedByThePackageOfTheMethod [
	"An extension method belongs to the package that extends the class: this test method's package is not kernel, while Object's is."

	self deny: (self deprecationIn: DeprecationTest >> #testDeprecatedPackageNames) isKernel.
	self assert: (self deprecationIn: Object >> #printString) isKernel
]

{ #category : 'tests - strategies' }
DeprecationTest >> testLogReactionNotifies [

	| notified |
	notified := false.
	self reaction: LogDeprecationReaction rewriting: NeverRewriteDeprecation during: [
		[ self runDefaultActionOfDeprecationsIn: [ ExampleForDeprecationTest new deprecatedMessageWithTransform ] ]
			on: DeprecationPerformedNotification
			do: [ :notification |
				notified := true.
				notification resume ] ].
	self assert: notified
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

{ #category : 'tests - strategies' }
DeprecationTest >> testNeverRewritingDoesNotTransform [

	self deny: (self callerIsTransformedWhenSending: #deprecatedMessageWithTransform rewriting: NeverRewriteDeprecation)
]

{ #category : 'tests' }
DeprecationTest >> testNoInfiniteLoopWhenShowWarningIsTrue [
	"Regression test for https://github.com/pharo-project/pharo/issues/19396"

	<ignoreNotImplementedSelectors: #( sendsDeprecatedMessageWithTransform )>
	| classFactory |
	classFactory := ClassFactoryForTestCase new.
	classFactory
		silentlyCompile: 'sendsDeprecatedMessageWithTransform ^ self deprecatedMessageWithTransform'
		in: ExampleForDeprecationTest.
	self reaction: LogDeprecationReaction rewriting: AlwaysRewriteDeprecation during: [
		[ ExampleForDeprecationTest new sendsDeprecatedMessageWithTransform ]
			on: Deprecation
			do: [ :deprecation | deprecation resume: deprecation defaultAction ] ].
	ExampleForDeprecationTest removeSelector: #sendsDeprecatedMessageWithTransform
]

{ #category : 'tests' }
DeprecationTest >> testTransformingDeprecation [

	self assert: (self callerIsTransformedWhenSending: #deprecatedMessageWithTransform)
]
