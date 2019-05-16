Class {
	#name : #HaltTest,
	#superclass : #TestCase,
	#category : #'Kernel-Tests-Objects'
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

{ #category : #'as yet unclassified' }
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

{ #category : #'tests - testing' }
HaltTest >> testHaltIfNil [
	<haltOrBreakpointForTesting>

	self should: [ nil haltIfNil] raise: Halt.
]
