Class {
	#name : 'RePointRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Migrated',
	#package : 'General-Rules-Tests',
	#tag : 'Migrated'
}

{ #category : 'tests' }
RePointRuleTest >> testRule [

	| critiques | 
	self class
		compile: 'methodTest ^Point x: 2 y: 3'
		classified: 'test-helper'.
	critiques := self myCritiquesOnMethod: self class >> #methodTest.
	self assert: critiques size equals: 1.
	[ "code of the test" ] ensure: [
		(self class >> #methodTest) removeFromSystem ]
]
