Class {
	#name : 'T2UsingTraitsWithSlotsTest',
	#superclass : 'T2AbstractTest',
	#category : 'TraitsV2-Tests',
	#package : 'TraitsV2-Tests'
}

{ #category : 'tests' }
T2UsingTraitsWithSlotsTest >> testRedefinitionKeepsSlots [

	| t1 c1 |
	t1 := self makeTrait: [ :aClassBuilder |
		      aClassBuilder
			      name: #T1;
			      slots: #( a b c ) ].
	t1 classTrait slots: #( aSlot ).
	c1 := self make: [ :aClassBuilder |
		      aClassBuilder
			      name: #C1;
			      traits: { t1 } ].

	self assertEmpty: c1 localSlots.
	self assertEmpty: c1 class localSlots.

	c1 addSlot: #d.

	self assertCollection: (c1 localSlots collect: [ :each | each name ]) hasSameElements: { #d }.
	self assertEmpty: c1 class localSlots
]
