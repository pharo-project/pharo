Class {
	#name : 'ReKeysDoRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Optimization',
	#package : 'General-Rules-Tests',
	#tag : 'Optimization'
}

{ #category : 'tests' }
ReKeysDoRuleTest >> testRuleNotViolatedWithKeys [

	| critiques | 
	self class compile: 'method: aDictionnary aDictionnary keysDo: [ :key | key ] ' classified: 'test-helper'.
	
	[ critiques := self myCritiquesOnMethod: self class >> #method:.
	self assertEmpty: critiques ] ensure: [ (self class >> #method:) removeFromSystem ]
]

{ #category : 'tests' }
ReKeysDoRuleTest >> testRuleNotViolatedWithValues [

	| critiques | 
	self class compile: 'method: aDictionnary aDictionnary valuesDo: [ :key | key ] ' classified: 'test-helper'.
	
	[ critiques := self myCritiquesOnMethod: self class >> #method:.
	self assertEmpty: critiques ] ensure: [ (self class >> #method:) removeFromSystem ]
]

{ #category : 'tests' }
ReKeysDoRuleTest >> testRuleWithKeys [

	| critiques | 
	self class compile: 'method: aDictionnary aDictionnary keys do: [ :key | key ] ' classified: 'test-helper'.
	
	[ critiques := self myCritiquesOnMethod: self class >> #method:.
	self assert: critiques size equals: 1 ] ensure: [ (self class >> #method:) removeFromSystem ]
]

{ #category : 'tests' }
ReKeysDoRuleTest >> testRuleWithValues [

	| critiques | 
	self class compile: 'method: aDictionnary aDictionnary values do: [ :key | key ] ' classified: 'test-helper'.
	
	[ critiques := self myCritiquesOnMethod: self class >> #method:.
	self assert: critiques size equals: 1 ] ensure: [ (self class >> #method:) removeFromSystem ]
]
