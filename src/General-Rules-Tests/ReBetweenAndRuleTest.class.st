Class {
	#name : 'ReBetweenAndRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Coding Idiom Violation',
	#package : 'General-Rules-Tests',
	#tag : 'Coding Idiom Violation'
}

{ #category : 'tests' }
ReBetweenAndRuleTest >> testRule [

	self generateMethod: 'a:a b:b c:c a >= b and: [a <= c]'.
	self assert: (self myCritiquesOnMethod: self class >> #a:b:c:) size equals: 1
]

{ #category : 'tests' }
ReBetweenAndRuleTest >> testRuleNotViolated [

	self generateMethod: 'a:a b:b c:c a between: b and: c'.
	self assertEmpty: (self myCritiquesOnMethod: self class >> #a:b:c:)
]
