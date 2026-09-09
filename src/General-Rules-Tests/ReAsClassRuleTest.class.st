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

	self assertEmpty: (self myCritiquesOnMethod: self class >> #testRuleNotViolated)
]

{ #category : 'tests' }
ReAsClassRuleTest >> testRuleWithAsClassIfAbsent [

	self assert: (self myCritiquesOnMethod: self class >> #methodWithAsClassIfPresent) size equals: 1
]

{ #category : 'tests' }
ReAsClassRuleTest >> testRuleWithAsClassIfPresent [

	self assert: (self myCritiquesOnMethod: self class >> #methodWithAsClassIfAbsent) size equals: 1
]
