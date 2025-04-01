Class {
	#name : 'HaltTest',
	#superclass : 'TestCase',
	#category : 'Debugging-Utils-Tests',
	#package : 'Debugging-Utils-Tests'
}

{ #category : 'helpers' }
HaltTest >> a [
	self b
]

{ #category : 'helpers' }
HaltTest >> a1 [
	self b1
]

{ #category : 'helpers' }
HaltTest >> b [
	<haltOrBreakpointForTesting>
	self haltIf: #testHaltIf
]

{ #category : 'helpers' }
HaltTest >> b1 [
	<haltOrBreakpointForTesting>
	self haltIf: #testasdasdfHaltIf
]

{ #category : 'tests - halt once' }
HaltTest >> differentCallSite: aBoolean [
	<haltOrBreakpointForTesting>
	| a |
	aBoolean 
		ifTrue: [ self haltOnce. a := 1 ]
		ifFalse: [ self haltOnce. a := 2 ].
	^ a
]

{ #category : 'helpers' }
HaltTest >> shouldHaltAfter: aNumber times: aBlock [
	
	self shouldHaltWhen: [ aNumber timesRepeat: aBlock ].
]

{ #category : 'helpers' }
HaltTest >> shouldHaltWhen: aBlock [

	self should: aBlock raise: Halt.
]

{ #category : 'helpers' }
HaltTest >> shouldntHaltAfter: aNumber times: aBlock [
	
	self shouldntHaltWhen: [ aNumber timesRepeat: aBlock ].
]

{ #category : 'helpers' }
HaltTest >> shouldntHaltWhen: aBlock [

	self shouldnt: aBlock raise: Halt.
]

{ #category : 'tests' }
HaltTest >> testContainsHalt [
	
	| annonClass |
	self 
		assert: (Object >> #halt) containsHalt;
		deny: (Object >> #haltIfNil) containsHalt;
		deny: (Halt class >> #once) containsHalt;
		assert: (self class>> #testHaltIfNil) containsHalt.
	
	annonClass := Object newAnonymousSubclass.
	annonClass 
		compile: 'm1 self halt';
		compile: 'm2 self haltIfNil';
		compile: 'm3 self yourself';
		compile: 'm4 Halt yourself';
		compile: 'm5 Halt once'.
	
	self
		assert: (annonClass >> #m1) containsHalt;
		assert: (annonClass >> #m2) containsHalt;
		deny: (annonClass >> #m3) containsHalt;
		deny: (annonClass >> #m4) containsHalt;
		assert: (annonClass >> #m5) containsHalt
]

{ #category : 'tests - halt on count' }
HaltTest >> testCounterFor [

	Halt during: [ 
		Halt initialize.
		self assert: (Halt counterFor: 123 andPc: 1) equals: 1. 
		self assert: (Halt counterFor: 123 andPc: 1) equals: 2. 
		self assert: (Halt counterFor: 123 andPc: 1) equals: 3. 
	
		self assert: (Halt counterFor: 123 andPc: 2) equals: 1. 
		self assert: (Halt counterFor: 123 andPc: 2) equals: 2. 
	
		self assert: (Halt counterFor: 124 andPc: 1) equals: 1 ]
]

{ #category : 'tests' }
HaltTest >> testHaltFromCountShouldContinueToHalt [

	<haltOrBreakpointForTesting>
	Halt during: [
		| anObject haltCount |
		Halt initialize.
		anObject := Object new.
		"Should continue halting after the count is reached"
		haltCount := 0.
		10 timesRepeat: [
			[ anObject haltFromCount: 3 ]
				on: Halt
				do: [ haltCount := haltCount + 1 ] ].
		self assert: haltCount equals: 8 ]
]

{ #category : 'tests' }
HaltTest >> testHaltFromCountShouldHaltIfExecutedEnoughTimes [

	<haltOrBreakpointForTesting>
	Halt during: [
		| anObject |
		Halt initialize.
		anObject := Object new.
		self shouldHaltAfter: 2 times: [ anObject haltFromCount: 2 ].

		"Should still halt if executed again after the count is reached"
		self shouldHaltAfter: 2 times: [ anObject haltFromCount: 2 ] ]
]

{ #category : 'tests' }
HaltTest >> testHaltFromCountShouldNotHaltIfExecutedEnoughTimes [

	<haltOrBreakpointForTesting>
	Halt during: [
		| anObject |
		Halt initialize.
		anObject := Object new.
		self shouldntHaltAfter: 2 times: [ anObject haltFromCount: 3 ] ]
]

{ #category : 'tests' }
HaltTest >> testHaltIf [
	<haltOrBreakpointForTesting>

	self shouldHaltWhen: [self haltIf: true].
	self shouldntHaltWhen: [self haltIf: false].

	self shouldHaltWhen: [self haltIf: [true]].
	self shouldntHaltWhen: [self haltIf: [false]].

	self shouldHaltWhen: [self haltIf: #testHaltIf].
	self shouldntHaltWhen: [self haltIf: #teadfasdfltIf].

	self shouldHaltWhen: [self a].
	self shouldntHaltWhen: [self a1].

	self shouldHaltWhen: [self haltIf: [:receiver | receiver class = self class]].
	self shouldntHaltWhen: [self haltIf: [:receiver | receiver class ~= self class]].
]

{ #category : 'tests' }
HaltTest >> testHaltIfNil [
	<haltOrBreakpointForTesting>

	self should: [ nil haltIfNil] raise: Halt
]

{ #category : 'tests - halt on count' }
HaltTest >> testHaltOnCountUsingHaltMessage2 [
	"On Count will only stop on the exact number not before not after"

	<haltOrBreakpointForTesting>
	| haltCount actionCount |
	Halt during: [
		Halt initializeOnCount.
		haltCount := 0.
		actionCount := 0.
		10 timesRepeat: [
			[
			Halt onCount: 3.
			actionCount := actionCount + 1 ]
				on: Halt
				do: [ haltCount := haltCount + 1 ] ].
		self assert: haltCount equals: 1.
		self assert: actionCount equals: 9 ]
]

{ #category : 'tests - halt on count' }
HaltTest >> testHaltOnCountUsingHaltMessageRaisesHaltOnlyOnce [

	<haltOrBreakpointForTesting>
	Halt during: [
		| haltCount actionCount |
		Halt initializeOnCount. "This is mandatory else running twice the test will fail."
		haltCount := 0.
		actionCount := 0.
		10 timesRepeat: [
			[
			Halt onCount: 3.
			actionCount := actionCount + 1 ]
				on: Halt
				do: [ haltCount := haltCount + 1 ] ].

		self assert: haltCount equals: 1.
		self assert: actionCount equals: 9 ]
]

{ #category : 'tests - halt on count' }
HaltTest >> testHaltOnCountUsingObjectHaltOnCount [

	<haltOrBreakpointForTesting>
	Halt during: [
		| anObject |
		Halt initializeOnCount. "This is mandatory else running twice the test will fail."
		anObject := Object new.
		self shouldntHaltAfter: 2 times: [ anObject haltOnCount: 3 ].
		self shouldHaltAfter: 3 times: [ anObject haltOnCount: 3 ] ]
]

{ #category : 'tests - halt on count' }
HaltTest >> testHaltOnCountUsingObjectHaltOnCountRaisesOnlyOnce [

	<haltOrBreakpointForTesting>
	Halt during: [
		| anObject haltCount |
		Halt initializeOnCount. "This is mandatory else running twice the test will fail."
		anObject := Object new.

		"Should stop halting after the count is reached"
		haltCount := 0.
		10 timesRepeat: [
			[ anObject haltOnCount: 3 ]
				on: Halt
				do: [ haltCount := haltCount + 1 ] ].
		self assert: haltCount equals: 1 ]
]

{ #category : 'tests - halt once' }
HaltTest >> testHaltOnce [

	<haltOrBreakpointForTesting>
	Halt during: [
		| anObject block |
		Halt initialize.
		anObject := Object new.
		block := [ anObject haltOnce ].
		self should: block raise: Halt.
		Halt resetOnce ]
]

{ #category : 'tests - halt once' }
HaltTest >> testHaltOnceTwoInARowShouldOnlyRaiseTheFirst [
	"during the first call in the execution will be stopped by the halt raised."

	<haltOrBreakpointForTesting>
	Halt during: [
		Halt resetOnce.
		self should: [ self differentCallSite: false ] raise: Halt.

		"The second call should not raise an halt."
		self
			shouldnt: [
				| res |
				res := self differentCallSite: false.
				self assert: res equals: 2 ]
			raise: Halt ]
]

{ #category : 'tests - halt once' }
HaltTest >> testHaltOnceTwoInDifferentCallSitesShouldRaiseHaltThenNot [
	"during the first call in the execution will be stopped by the halt raised."

	<haltOrBreakpointForTesting>
	Halt during: [
		Halt resetOnce.
		self should: [ self differentCallSite: false ] raise: Halt.
		self should: [ self differentCallSite: true ] raise: Halt.

		"The second call should not raise an halt."
		self
			shouldnt: [
				| res |
				res := self differentCallSite: false.
				self assert: res equals: 2 ]
			raise: Halt.
		self
			shouldnt: [
				| res |
				res := self differentCallSite: true.
				self assert: res equals: 1 ]
			raise: Halt ]
]

{ #category : 'tests' }
HaltTest >> testHaltUpToShouldNotHaltAfterTimes [

	<haltOrBreakpointForTesting>
	Halt during: [
		| anObject count |
		Halt initialize.
		anObject := Object new.
		count := 0.
		self
			should: [
				3 timesRepeat: [
					anObject haltFromCount: 2.
					count := count + 1 ] ]
			raise: Halt.
		self assert: count equals: 1 ]
]

{ #category : 'tests' }
HaltTest >> testHaltUpToShouldNotHaltAfterTimes2 [

	<haltOrBreakpointForTesting>
	Halt during: [
		| anObject count haltCount |
		Halt initialize.
		anObject := Object new.
		count := 0.
		haltCount := 0.
		[
		5 timesRepeat: [
			Halt upTo: 2.
			count := count + 1 ] ]
			on: Halt
			do: [ :ex |
				haltCount := haltCount + 1.
				ex resume ].
		self assert: count equals: 5.
		self assert: haltCount equals: 2 ]
]

{ #category : 'tests' }
HaltTest >> testHaltUpToShouldOnlyTwoTimes [

	<haltOrBreakpointForTesting>
	Halt during: [ 
	| anObject haltCount |
	Halt initialize.
	anObject := Object new.
	"Should continue halting after the count is reached"
	haltCount := 0.
	10 timesRepeat: [
		[ anObject haltUpTo: 2 ]
			on: Halt
			do: [ haltCount := haltCount + 1 ] ].
	self assert: haltCount equals: 2 ]
]

{ #category : 'tests - halt once' }
HaltTest >> testOnce [

	<haltOrBreakpointForTesting>
	Halt during: [
		Halt initialize.
		self should: [ Halt once ] raise: Halt.
		Halt resetOnce ]
]
