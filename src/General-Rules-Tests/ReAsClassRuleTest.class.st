Class {
	#name : 'ReAsClassRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Design Flaws',
	#package : 'General-Rules-Tests',
	#tag : 'Design Flaws'
}

{ #category : 'test-help' }
ReAsClassRuleTest >> methodWithAsClassIfAbsent [

	self class asClassIfAbsent: [  ]
]

{ #category : 'test-help' }
ReAsClassRuleTest >> methodWithAsClassIfPresent [

	 self class asClassIfPresent: [  ]
]

{ #category : 'tests' }
ReAsClassRuleTest >> testRuleNotViolated [

	| critiques |
	critiques := self myCritiquesOnMethod:
		             self class >> #testRuleNotViolated.
	self assertEmpty: critiques 
]

{ #category : 'tests' }
ReAsClassRuleTest >> testRuleWithAsClassIfAbsent [

	| critiques |
	critiques := self myCritiquesOnMethod: 
		             self class >> #methodWithAsClassIfPresent.
	self assert: critiques size equals: 1
]

{ #category : 'tests' }
ReAsClassRuleTest >> testRuleWithAsClassIfPresent [

	| critiques |
	critiques := self myCritiquesOnMethod: self class >> #methodWithAsClassIfAbsent.
	self assert: critiques size equals: 1
]
