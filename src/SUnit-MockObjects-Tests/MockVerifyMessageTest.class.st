Class {
	#name : 'MockVerifyMessageTest',
	#superclass : 'TestCase',
	#instVars : [
		'mock'
	],
	#category : 'SUnit-MockObjects-Tests',
	#package : 'SUnit-MockObjects-Tests'
}

{ #category : 'running' }
MockVerifyMessageTest >> setUp [

	super setUp.
	mock := MockObject new.
	mock
		on: #meaningOfLife: 
		with: 12
		verify: [ :arg | arg even ]
]

{ #category : 'running' }
MockVerifyMessageTest >> testVerify [
	<ignoreNotImplementedSelectors: #( meaningOfLife: )>
	
	mock meaningOfLife: 3.
	self should: [ self verify: mock] raise: TestFailure
]

{ #category : 'running' }
MockVerifyMessageTest >> testVerify2 [
	"When the arguments are correct, the verify block is executed and it should return true."

	<ignoreNotImplementedSelectors: #( meaningOfLife: )>
	
	mock meaningOfLife: 12.
	self verify: mock
]
