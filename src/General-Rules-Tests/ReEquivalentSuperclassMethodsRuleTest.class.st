Class {
	#name : 'ReEquivalentSuperclassMethodsRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Migrated',
	#package : 'General-Rules-Tests',
	#tag : 'Migrated'
}

{ #category : 'utilities' }
ReEquivalentSuperclassMethodsRuleTest >> myCritiques [

	| critiques |
	critiques := OrderedCollection new.
	self subjectUnderTest new
		check: self class >> #sampleMethod
		forCritiquesDo: [ :critique | critiques add: critique ].
	^ critiques
]

{ #category : 'tests' }
ReEquivalentSuperclassMethodsRuleTest >> testRule [

	| critiques |
	critiques := self myCritiquesOnMethod: self class >> #myCritiques.
	self assert: critiques size equals: 1
]
