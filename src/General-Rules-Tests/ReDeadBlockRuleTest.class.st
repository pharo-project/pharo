Class {
	#name : 'ReDeadBlockRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Optimization',
	#package : 'General-Rules-Tests',
	#tag : 'Optimization'
}

{ #category : 'tests' }
ReDeadBlockRuleTest >> testRule [

	self generateMethod: 'method [ self foo ]'.
	self assert: (self myCritiquesOnMethod: self class >> #method) size equals: 1
]

{ #category : 'tests' }
ReDeadBlockRuleTest >> testRuleNotViolatedIfBlockAssigned [

	self generateMethod: 'method | temp |  temp := [ self foo ] '.
	self assertEmpty: (self myCritiquesOnMethod: self class >> #method)
]

{ #category : 'tests' }
ReDeadBlockRuleTest >> testRuleNotViolatedIfMessageSendToIt [

	self generateMethod: 'method [ self foo ] value '.
	self assertEmpty: (self myCritiquesOnMethod: self class >> #method)
]

{ #category : 'tests' }
ReDeadBlockRuleTest >> testRuleNotViolatedIfReturned [

	self generateMethod: 'method ^ [ self foo ] '.
	self assertEmpty: (self myCritiquesOnMethod: self class >> #method)
]
