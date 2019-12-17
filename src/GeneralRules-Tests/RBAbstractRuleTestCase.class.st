Class {
	#name : #RBAbstractRuleTestCase,
	#superclass : #TestCase,
	#category : #'GeneralRules-Tests'
}

{ #category : #'test-support' }
RBAbstractRuleTestCase >> critiguesFor: ruleClass onMethod: method [
	| critiques |
	critiques := OrderedCollection new.
	ruleClass new 
		check: method forCritiquesDo:[:critique | critiques add: critique].
	^critiques 
]

{ #category : #'test-support' }
RBAbstractRuleTestCase >> myCritiguesOnMethod: method [
	| critiques |
	critiques := OrderedCollection new.
	self subjectUnderTest  new 
		check: method forCritiquesDo:[:critique | critiques add: critique].
	^critiques 
]

{ #category : #'test-support' }
RBAbstractRuleTestCase >> myCritiques [
	| critiques |
	critiques := OrderedCollection new.
	self subjectUnderTest  new 
		check: (self class >> #sampleMethod ) forCritiquesDo:[:critique | critiques add: critique].
	^critiques 
]

{ #category : #'test-support' }
RBAbstractRuleTestCase >> sourceAtChritique: critique [
	^ critique sourceAnchor in: [ :anchor | anchor entity sourceCode atAll: anchor interval ]
]

{ #category : #'test-support' }
RBAbstractRuleTestCase >> subjectUnderTest [ 
	^(self class name allButLast: 4) asClass
]
