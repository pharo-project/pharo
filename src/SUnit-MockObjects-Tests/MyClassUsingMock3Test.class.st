Class {
	#name : 'MyClassUsingMock3Test',
	#superclass : 'TestCase',
	#instVars : [
		'mock'
	],
	#category : 'SUnit-MockObjects-Tests',
	#package : 'SUnit-MockObjects-Tests'
}

{ #category : 'running' }
MyClassUsingMock3Test >> setUp [

	super setUp.
	mock := MockObject new.
	mock
		on: #meaningOfLife: 
		with: 12
		verify: [ :arg | arg even ]
]

{ #category : 'running' }
MyClassUsingMock3Test >> testVerify [

	<ignoreNotImplementedSelectors: #( meaningOfLife: )>
	mock meaningOfLife: 3
]

{ #category : 'running' }
MyClassUsingMock3Test >> testVerify2 [
	"When the arguments are correct, the verify block is executed and it should return true."

	<ignoreNotImplementedSelectors: #( meaningOfLife: )>
	mock meaningOfLife: 12.
	self verify: mock
]
