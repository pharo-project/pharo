"
Just a class used for tests, it has a complex trait combination.
"
Class {
	#name : #HETeSimpleClassWithComplexTraits,
	#superclass : #HETeSimpleClass,
	#traits : 'THEOneTestTrait - {#oneMessageRequired} + (THEOtherTestTrait @ {#otherMessage->#otherMessage2})',
	#classTraits : 'THEOneTestTrait classTrait + THEOtherTestTrait classTrait',
	#category : 'Hermes-Tests-Test-Classes'
}
