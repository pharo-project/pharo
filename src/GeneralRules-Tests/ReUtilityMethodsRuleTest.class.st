Class {
	#name : #ReUtilityMethodsRuleTest,
	#superclass : #ReAbstractRuleTestCase,
	#category : #'GeneralRules-Tests-Migrated'
}

{ #category : #accessing }
ReUtilityMethodsRuleTest >> myCritiques [
	| critiques |
	critiques := OrderedCollection new.
	self subjectUnderTest  new 
		check: (self class >> #sampleMethod: ) forCritiquesDo:[:critique | critiques add: critique].
	^critiques 
]

{ #category : #sample }
ReUtilityMethodsRuleTest >> sampleMethod: arg [
	^arg
]

{ #category : #tests }
ReUtilityMethodsRuleTest >> testRule [
	| critiques|
 	critiques := self myCritiques.

 	self assert: critiques size equals: 1.
 	self assert: critiques first rule class equals: ReUtilityMethodsRule
]
