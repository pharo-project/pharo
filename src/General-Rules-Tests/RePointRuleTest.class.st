Class {
	#name : 'RePointRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Optimization',
	#package : 'General-Rules-Tests',
	#tag : 'Optimization'
}

{ #category : 'tests' }
RePointRuleTest >> testRule [

	| critiques | 
	self class
		compile: 'methodTest ^Point x: 2 y: 3'
		classified: 'test-helper'.
	critiques := self myCritiquesOnMethod: self class >> #methodTest.
	self assert: critiques size equals: 1.
	[ "code of the test" ] ensure: [
		(self class >> #methodTest) removeFromSystem ]
]

{ #category : 'tests' }
RePointRuleTest >> testRuleNotViolated [

	| critiques |
	self class compile: 'x:x y:y ^ x + y' classified: 'test-helper'.
	self class
		compile: 'methodTest ^self new x: 2 y: 3'
		classified: 'test-helper'.
	critiques := self myCritiquesOnMethod: self class >> #methodTest.
	self assertEmpty: critiques.
	[ "code of the test" ] ensure: [
		(self class >> #methodTest) removeFromSystem ].
	[ "code of the test" ] ensure: [
		(self class >> #x:y:) removeFromSystem ]
]
