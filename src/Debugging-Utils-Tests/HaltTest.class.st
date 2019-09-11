Class {
	#name : #HaltTest,
	#superclass : #TestCase,
	#category : #'Debugging-Utils-Tests'
}

{ #category : #helpers }
HaltTest >> a [
	self b
]

{ #category : #helpers }
HaltTest >> a1 [
	self b1
]

{ #category : #helpers }
HaltTest >> b [
	<haltOrBreakpointForTesting>
	self haltIf: #testHaltIf
]

{ #category : #helpers }
HaltTest >> b1 [
	<haltOrBreakpointForTesting>
	self haltIf: #testasdasdfHaltIf
]

{ #category : #helpers }
HaltTest >> shouldHaltAfter: aNumber times: aBlock [
	
	self shouldHaltWhen: [ aNumber timesRepeat: aBlock ].
]

{ #category : #helpers }
HaltTest >> shouldHaltWhen: aBlock [

	self should: aBlock raise: Halt.
]

{ #category : #helpers }
HaltTest >> shouldntHaltAfter: aNumber times: aBlock [
	
	self shouldntHaltWhen: [ aNumber timesRepeat: aBlock ].
]

{ #category : #helpers }
HaltTest >> shouldntHaltWhen: aBlock [

	self shouldnt: aBlock raise: Halt.
]

{ #category : #tests }
HaltTest >> testContainsHalt [
	
	| annonClass |
	
	self 
	deny: (Object >> #halt) containsHalt;
	deny: (Object >> #haltIfNil) containsHalt;
	deny: (Halt class >> #once) containsHalt;
	assert: (self class>> #testHaltIfNil) containsHalt.
	
	annonClass := Object newAnonymousSubclass.
	annonClass 
	compile: 'm1 self halt';
	compile: 'm2 self haltIfNil';
	compile: 'm3 self yourself'.
	
	self
	assert: (annonClass >> #m1) containsHalt;
	assert: (annonClass >> #m2) containsHalt;
	deny: (annonClass >> #m3) containsHalt.
	
	
]

{ #category : #tests }
HaltTest >> testHaltFromCount [
	<haltOrBreakpointForTesting>

	| anObject haltCount |
	anObject := Object new.
	thisContext method recompile. "reset state of #halOnce here"
	"Halts regardless of whether HaltOnce is false for the image"
	self shouldHaltWhen: [ anObject haltFromCount: 1 ].

	"Should halt if executed enough times"	
	self shouldHaltAfter: 2 times: [ anObject haltFromCount: 2 ].
	
	"Should still halt if executed again after the count is reached"	
	self shouldHaltAfter: 2 times: [ anObject haltFromCount: 2 ].
	
	"Should not halt if not executed enough times"	
	self shouldntHaltAfter: 2 times: [ anObject haltFromCount: 3 ].
	
	"Should continue halting after the count is reached"
	haltCount := 0.
	10 timesRepeat: [ [ anObject haltFromCount: 3 ]
				on: Halt
				do: [ haltCount := haltCount + 1 ] ].
	self assert: haltCount equals: 8

]

{ #category : #tests }
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

{ #category : #tests }
HaltTest >> testHaltIfNil [
	<haltOrBreakpointForTesting>

	self should: [ nil haltIfNil] raise: Halt.
]

{ #category : #tests }
HaltTest >> testHaltOnCount [
	<haltOrBreakpointForTesting>

	| anObject haltCount |
	anObject := Object new.
	thisContext method recompile.

	"Should not halt if not executed enough times"
	self shouldntHaltAfter: 2 times: [ anObject haltOnCount: 3 ].
	self shouldHaltAfter: 3 times: [ anObject haltOnCount: 3 ].

	"Should stop halting after the count is reached"
	haltCount := 0.
	10 timesRepeat: [ [ anObject haltOnCount: 3 ]
				on: Halt
				do: [ haltCount := haltCount + 1 ] ].
	self assert: haltCount equals: 1
]

{ #category : #tests }
HaltTest >> testHaltOnce [
	<haltOrBreakpointForTesting>
	| anObject block |
	anObject := Object new.
	block :=  [ anObject haltOnce ]. 
	self should: block raise: Halt.
	"block value."
	Halt resetOnce.
]
