Class {
	#name : 'ReInconsistentMethodClassificationRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Migrated',
	#package : 'General-Rules-Tests',
	#tag : 'Migrated'
}

{ #category : 'accessing' }
ReInconsistentMethodClassificationRuleTest >> initialize [ 
]

{ #category : 'initalize' }
ReInconsistentMethodClassificationRuleTest >> myCritiques [ 
^ super myCritiques. 
]

{ #category : 'tests' }
ReInconsistentMethodClassificationRuleTest >> testRule [
	|critiques|
	critiques := self myCritiquesOnMethod: self class >> #myCritiques.
	self assert: critiques size equals: 1.
	critiques := self myCritiquesOnMethod: self class >> #initialize.
	self assert: critiques size equals: 1.
]
