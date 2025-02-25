Class {
	#name : 'ReKeysDoRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Optimization',
	#package : 'General-Rules-Tests',
	#tag : 'Optimization'
}

{ #category : 'tests' }
ReKeysDoRuleTest >> testRuleNotViolatedWithKeys [

	self generateMethod: 'method: aDictionnary aDictionnary keysDo: [ :key | key ] '.
	self assertEmpty: (self myCritiquesOnMethod: self class >> #method:)
]

{ #category : 'tests' }
ReKeysDoRuleTest >> testRuleNotViolatedWithValues [

	self generateMethod: 'method: aDictionnary aDictionnary valuesDo: [ :key | key ] '.
	self assertEmpty: (self myCritiquesOnMethod: self class >> #method:)
]

{ #category : 'tests' }
ReKeysDoRuleTest >> testRuleWithKeys [

	self generateMethod: 'method: aDictionnary aDictionnary keys do: [ :key | key ] '.
	self assert: (self myCritiquesOnMethod: self class >> #method:) size equals: 1
]

{ #category : 'tests' }
ReKeysDoRuleTest >> testRuleWithValues [

	self generateMethod: 'method: aDictionnary aDictionnary values do: [ :key | key ] '.
	self assert: (self myCritiquesOnMethod: self class >> #method:) size equals: 1
]
