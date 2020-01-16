Class {
	#name : #ReAbstractRuleTestCase,
	#superclass : #TestCase,
	#category : #'GeneralRules-Tests-Migrated'
}

{ #category : #'test-support' }
ReAbstractRuleTestCase >> critiguesFor: ruleClass onMethod: method [
	| critiques |
	critiques := OrderedCollection new.
	ruleClass new 
		check: method forCritiquesDo:[:critique | critiques add: critique].
	^critiques 
]

{ #category : #'test-support' }
ReAbstractRuleTestCase >> myCritiguesOnMethod: method [
	| critiques |
	critiques := OrderedCollection new.
	self subjectUnderTest  new 
		check: method forCritiquesDo:[:critique | critiques add: critique].
	^critiques 
]

{ #category : #'test-support' }
ReAbstractRuleTestCase >> myCritiques [
	| critiques |
	critiques := OrderedCollection new.
	self subjectUnderTest  new 
		check: (self class >> #sampleMethod ) forCritiquesDo:[:critique | critiques add: critique].
	^critiques 
]

{ #category : #'test-support' }
ReAbstractRuleTestCase >> sourceAtChritique: critique [
	^ critique sourceAnchor in: [ :anchor | anchor entity sourceCode atAll: anchor interval ]
]

{ #category : #'test-support' }
ReAbstractRuleTestCase >> subjectUnderTest [ 
	^(self class name allButLast: 4) asClass
]
