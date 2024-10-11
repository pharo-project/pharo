Class {
	#name : 'ReDeadBlockRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Optimization',
	#package : 'General-Rules-Tests',
	#tag : 'Optimization'
}

{ #category : 'tests' }
ReDeadBlockRuleTest >> testRule [ 

	| critiques | 
	
	self class compile: 'method [ self foo ]' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #method.
	self assert: critiques size equals: 1 ] ensure: [ (self class >> #method) removeFromSystem ]
]

{ #category : 'tests' }
ReDeadBlockRuleTest >> testRuleNotViolatedIfBlockAssigned [

	| critiques | 
	
	self class compile: 'method | temp |  temp := [ self foo ] ' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #method.
	self assertEmpty: critiques  ] ensure: [ (self class >> #method) removeFromSystem ]
]

{ #category : 'tests' }
ReDeadBlockRuleTest >> testRuleNotViolatedIfMessageSendToIt [

	| critiques | 
	
	self class compile: 'method [ self foo ] value ' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #method.
	self assertEmpty: critiques  ] ensure: [ (self class >> #method) removeFromSystem ]
]

{ #category : 'tests' }
ReDeadBlockRuleTest >> testRuleNotViolatedIfReturned [

	| critiques | 
	
	self class compile: 'method ^ [ self foo ] ' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #method.
	self assertEmpty: critiques  ] ensure: [ (self class >> #method) removeFromSystem ]
]
