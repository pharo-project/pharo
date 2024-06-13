Class {
	#name : 'ReUseSetUpRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Design Flaws',
	#package : 'General-Rules-Tests',
	#tag : 'Design Flaws'
}

{ #category : 'tests' }
ReUseSetUpRuleTest >> testRule [

	| critiques |
	self class compile: 'initialize' classified: 'initialization'.
	[ critiques := self myCritiquesOnMethod: self class >> #initialize.
	self assert: critiques size equals: 1 ] ensure: [
		(self class >> #initialize) removeFromSystem ]
]

{ #category : 'tests' }
ReUseSetUpRuleTest >> testRuleNotViolated [

	| critiques |
	self class compile: 'setUp'.
	[ critiques := self myCritiquesOnClass: self class.
	self assertEmpty: critiques ] ensure: [
		(self class >> #setUp) removeFromSystem ]
]
