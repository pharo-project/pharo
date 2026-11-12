"
I check how a script of Metacello image or Metacello registry treats the baseline it names.
"
Class {
	#name : 'MetacelloScriptExecutorTest',
	#superclass : 'MetacelloTestCase',
	#category : 'Metacello-Tests-Specs',
	#package : 'Metacello-Tests',
	#tag : 'Specs'
}

{ #category : 'tests' }
MetacelloScriptExecutorTest >> testARegistryScriptOnABaselineNotRegisteredIsAnError [

	self
		should: [
				Metacello registry
					baseline: 'TestNotRegistered';
					lock ]
		raise: NotFound
]

{ #category : 'tests' }
MetacelloScriptExecutorTest >> testARegistryScriptOnARegisteredBaselineAnswersItsSpec [

	Metacello new
		baseline: 'TestToLock2';
		repository: 'http://repo.com';
		register.

	self
		assert: (Metacello registry
				 baseline: 'TestToLock2';
				 list) name
		equals: 'TestToLock2'
]

{ #category : 'tests' }
MetacelloScriptExecutorTest >> testAnImageScriptOnABaselineNotLoadedIsAnError [

	Metacello new
		baseline: 'TestToLock2';
		repository: 'http://repo.com';
		register.

	self
		should: [
				Metacello image
					baseline: 'TestToLock2';
					lock ]
		raise: NotFound
]
