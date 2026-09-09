Class {
	#name : 'ReInconsistentMethodClassificationRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Design Flaws',
	#package : 'General-Rules-Tests',
	#tag : 'Design Flaws'
}

{ #category : 'tests' }
ReInconsistentMethodClassificationRuleTest >> testRule [

	self generateMethod: 'myCritiques super myCritiques' classified: 'accessing'.
	self assert: (self myCritiquesOnMethod: self class >> #myCritiques) size equals: 1.
	self generateMethod: 'initialize super initialize' classified: 'accessing'.
	self assert: (self myCritiquesOnMethod: self class >> #initialize) size equals: 1
]

{ #category : 'tests' }
ReInconsistentMethodClassificationRuleTest >> testRuleNotViolated [

	self generateMethod: 'initialize super initialize' classified: 'initialization'.
	self assertEmpty: (self myCritiquesOnMethod: self class >> #initialize)
]
