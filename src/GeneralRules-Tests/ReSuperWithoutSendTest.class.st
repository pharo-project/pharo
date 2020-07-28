Class {
	#name : #ReSuperWithoutSendTest,
	#superclass : #TestCase,
	#category : #'GeneralRules-Tests-Migrated'
}

{ #category : #tests }
ReSuperWithoutSendTest >> testBasicCheck [

	| ast |
	ast :=  self class compiler parse: 'testMethod ^super'.
	ast doSemanticAnalysis.
	self assert: (ReSuperWithoutSend new basicCheck: ast variableNodes first).
	self deny: (ReSuperWithoutSend new basicCheck: ast statements first).
	ast := self class compiler parse: 'testMethod ^ super doit'.
	ast doSemanticAnalysis.
	self deny: (ReSuperWithoutSend new basicCheck: ast variableNodes first).
	ast := self class compiler parse: 'testMethod ^ self doit: super'.
	ast doSemanticAnalysis.
	self assert: (ReSuperWithoutSend new basicCheck: ast variableNodes second).
]
