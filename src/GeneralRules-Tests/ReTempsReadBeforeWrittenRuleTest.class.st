Class {
	#name : #ReTempsReadBeforeWrittenRuleTest,
	#superclass : #ReAbstractRuleTestCase,
	#category : #'GeneralRules-Tests-Migrated'
}

{ #category : #helpers }
ReTempsReadBeforeWrittenRuleTest >> mockMethod [
	| temp1 temp2 temp3 |
	temp3 := 1.
	[ temp2 := 2 ] value.
	^ { temp1 . temp2 . temp3 }
]

{ #category : #tests }
ReTempsReadBeforeWrittenRuleTest >> testFoundTemporaries [

	| critiques |
	critiques := self myCritiquesOnMethod: self class >> #mockMethod.

	self assert: critiques size equals: 1.
	self
		assertCollection: critiques first rule temps
		hasSameElements: #( temp1 temp2 )
]
