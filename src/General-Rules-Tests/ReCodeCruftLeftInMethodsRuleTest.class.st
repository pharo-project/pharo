Class {
	#name : 'ReCodeCruftLeftInMethodsRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Migrated',
	#package : 'General-Rules-Tests',
	#tag : 'Migrated'
}

{ #category : 'tests' }
ReCodeCruftLeftInMethodsRuleTest >> testRule [

	| critiques |
	self class compile: 'method self halt' classified: 'test-helper'.
	[
	critiques := self myCritiquesOnMethod: self class >> #method.
	self assert: critiques size equals: 1 ] ensure: [
		(self class >> #method) removeFromSystem ]
]

{ #category : 'tests' }
ReCodeCruftLeftInMethodsRuleTest >> testRuleNotViolated [

	| critiques |
	self class
		compile: 'method  <debuggerCompleteToSender> self halt'
		classified: 'test-helper'.
	[
	critiques := self critiguesFor: ReCodeCruftLeftInMethodsRule onMethod: self class >> #method.
	self assertEmpty: critiques ] ensure: [
		(self class >> #method) removeFromSystem ]
]
