Class {
	#name : 'MockUnorderedObjectTest',
	#superclass : 'TestCase',
	#instVars : [
		'mock'
	],
	#category : 'SUnit-MockObjects-Tests',
	#package : 'SUnit-MockObjects-Tests'
}

{ #category : 'running' }
MockUnorderedObjectTest >> setUp [

	super setUp.
	mock := MockUnorderedObject new.
	mock
		on: #meaningOfLife 
		respond: 42. 
	mock 
		on: #secondMeaning
		respond: 84.
	
]

{ #category : 'running' }
MockUnorderedObjectTest >> testMeaningOfLife [

	<ignoreNotImplementedSelectors: #(meaningOfLife secondMeaning)>
	
	self assert: mock meaningOfLife equals: 42.
	self assert: mock secondMeaning equals: 84
]
