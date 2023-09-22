Class {
	#name : 'RedTest',
	#superclass : 'MockTest',
	#classInstVars : [
		'ignoreTest'
	],
	#category : 'TestRunner-Tests-Utilities-Test',
	#package : 'TestRunner-Tests',
	#tag : 'Utilities-Test'
}

{ #category : 'tests' }
RedTest >> testFail [

	self shouldBeIgnored ifTrue: [ ^ self ].
	self assert: false
]

{ #category : 'tests' }
RedTest >> testPass [

	self skipOnPharoCITestingEnvironment.
	self assert: true
]

{ #category : 'tests' }
RedTest >> testWithError [

	self shouldBeIgnored ifTrue: [ ^ self ].
	self assert: nil
]
