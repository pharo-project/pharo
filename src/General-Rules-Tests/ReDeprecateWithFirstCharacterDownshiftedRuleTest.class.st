Class {
	#name : 'ReDeprecateWithFirstCharacterDownshiftedRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Style',
	#package : 'General-Rules-Tests',
	#tag : 'Style'
}

{ #category : 'tests' }
ReDeprecateWithFirstCharacterDownshiftedRuleTest >> testRule [ 

	| critiques | 
	self class compile: 'method: aString ^ aString withFirstCharacterDownshifted' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #method:.
	self assert: critiques size equals: 1 ] ensure: [ (self class >> #method:) removeFromSystem ]
]

{ #category : 'tests' }
ReDeprecateWithFirstCharacterDownshiftedRuleTest >> testRuleNotViolated [

	| critiques | 
	self class compile: 'method: aString ^ aString uncapitalized' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #method:.
	self assertEmpty: critiques ] ensure: [ (self class >> #method:) removeFromSystem ]
]
