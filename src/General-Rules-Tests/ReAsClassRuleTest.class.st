Class {
	#name : 'ReAsClassRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#classInstVars : [
		'critiques'
	],
	#category : 'General-Rules-Tests-Migrated',
	#package : 'General-Rules-Tests',
	#tag : 'Migrated'
}

{ #category : 'converting' }
ReAsClassRuleTest >> myMethodCallingAsClass [
	 ^ #ReAsClassRuleTest asClass
]

{ #category : 'converting' }
ReAsClassRuleTest >> myMethodCallingAsClassIfAbsent [
	 ^ #ReAsClassRuleTest asClassIfAbsent: [].
]

{ #category : 'converting' }
ReAsClassRuleTest >> myMethodCallingAsClassIfPresents [
	 ^ #ReAsClassRuleTest asClassIfPresent: [].
]

{ #category : 'tests' }
ReAsClassRuleTest >> testRule [ 
	|critiques|
	critiques := self myCritiquesOnMethod: self myMethodCallingAsClassIfPresents.
	self assert: critiques size equals: 1
]
