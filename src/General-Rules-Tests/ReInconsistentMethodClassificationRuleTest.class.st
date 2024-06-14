Class {
	#name : 'ReInconsistentMethodClassificationRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Design Flaws',
	#package : 'General-Rules-Tests',
	#tag : 'Design Flaws'
}

{ #category : 'tests' }
ReInconsistentMethodClassificationRuleTest >> testRule [

	| critiques |
	self class
		compile: 'myCritiques super myCritiques'
		classified: 'accessing'.
	[ critiques := self myCritiquesOnMethod: self class >> #myCritiques.
	self assert: critiques size equals: 1 ] ensure: [
		(self class >> #myCritiques) removeFromSystem ].
	self class
		compile: 'initialize super initialize'
		classified: 'accessing'.
	[ critiques := self myCritiquesOnMethod: self class >> #initialize.
	self assert: critiques size equals: 1 ] ensure: [
		(self class >> #initialize) removeFromSystem ]
]

{ #category : 'tests' }
ReInconsistentMethodClassificationRuleTest >> testRuleNotViolated [

	| critiques |
	self class
		compile: 'initialize super initialize'
		classified: 'initialization'.
	[ critiques := self myCritiquesOnMethod: self class >> #initialize.
	self assertEmpty: critiques ] ensure: [
		(self class >> #initialize) removeFromSystem ]
]
