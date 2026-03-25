Class {
	#name : 'RedTest',
	#superclass : 'MockTest',
	#classInstVars : [
		'ignoreTest'
	],
	#category : 'TestRunner-FakeTests',
	#package : 'TestRunner-FakeTests'
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
