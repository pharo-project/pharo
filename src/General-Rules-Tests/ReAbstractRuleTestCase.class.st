Class {
	#name : 'ReAbstractRuleTestCase',
	#superclass : 'TestCase',
	#category : 'General-Rules-Tests-Migrated',
	#package : 'General-Rules-Tests',
	#tag : 'Migrated'
}

{ #category : 'testing' }
ReAbstractRuleTestCase class >> isAbstract [

	^self == ReAbstractRuleTestCase
]

{ #category : 'utilities' }
ReAbstractRuleTestCase >> critiguesFor: ruleClass onMethod: method [
	| critiques |
	critiques := OrderedCollection new.
	ruleClass new
		check: method forCritiquesDo:[:critique | critiques add: critique].
	^critiques
]

{ #category : 'utilities' }
ReAbstractRuleTestCase >> myCritiques [
	| critiques |
	critiques := OrderedCollection new.
	self subjectUnderTest  new
		check: (self class >> #sampleMethod ) forCritiquesDo:[:critique | critiques add: critique].
	^critiques
]

{ #category : 'utilities' }
ReAbstractRuleTestCase >> myCritiquesOnClass: aClass [
	| critiques |
	critiques := OrderedCollection new.
	self subjectUnderTest new
		check: aClass forCritiquesDo:[:critique | critiques add: critique].
	^critiques
]

{ #category : 'utilities' }
ReAbstractRuleTestCase >> myCritiquesOnMethod: method [
	| critiques |
	critiques := OrderedCollection new.
	self subjectUnderTest  new
		check: method forCritiquesDo:[:critique | critiques add: critique].
	^critiques
]

{ #category : 'utilities' }
ReAbstractRuleTestCase >> sourceAtChritique: critique [

	| anchor |
	anchor := critique sourceAnchor.
	^ anchor entity sourceCode atAll: anchor interval
]

{ #category : 'utilities' }
ReAbstractRuleTestCase >> subjectUnderTest [

	^ (self class name allButLast: 4) asClassInEnvironment:
		  self class environment
]
