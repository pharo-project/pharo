Class {
	#name : #RBAbstractRuleTestCase,
	#superclass : #TestCase,
	#category : #'GeneralRules-Tests'
}

{ #category : #'as yet unclassified' }
RBAbstractRuleTestCase >> critiguesFor: ruleClass onMethod: method [
	| critiques |
	critiques := OrderedCollection new.
	ruleClass new 
		check: method forCritiquesDo:[:critique | critiques add: critique].
	^critiques 
]

{ #category : #'as yet unclassified' }
RBAbstractRuleTestCase >> myCritiguesOnMethod: method [
	| critiques |
	critiques := OrderedCollection new.
	self subjectUnderTest  new 
		check: method forCritiquesDo:[:critique | critiques add: critique].
	^critiques 
]

{ #category : #'as yet unclassified' }
RBAbstractRuleTestCase >> myCritiquesOnFoo [
	| critiques |
	critiques := OrderedCollection new.
	self subjectUnderTest  new 
		check: (self class >> #foo ) forCritiquesDo:[:critique | critiques add: critique].
	^critiques 
]

{ #category : #'as yet unclassified' }
RBAbstractRuleTestCase >> subjectUnderTest [ 
	^(self class name allButLast: 4) asClass
]
