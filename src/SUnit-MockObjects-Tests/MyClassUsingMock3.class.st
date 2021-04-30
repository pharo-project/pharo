Class {
	#name : #MyClassUsingMock3,
	#superclass : #TestCase,
	#instVars : [
		'mock'
	],
	#category : #'SUnit-MockObjects-Tests'
}

{ #category : #running }
MyClassUsingMock3 >> setUp [

	super setUp.
	mock := MockObject new.
	mock
		on: #meaningOfLife: 
		with: 12
		verify: [ :arg | arg even ]
]

{ #category : #running }
MyClassUsingMock3 >> testVerify [

	mock meaningOfLife: 3.
]

{ #category : #running }
MyClassUsingMock3 >> testVerify2 [
	"When the arguments are correct, the verify block is executed and it should return true."
	
	mock meaningOfLife: 12.
	self verify: mock.
]
