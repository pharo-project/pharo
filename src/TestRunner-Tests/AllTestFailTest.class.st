Class {
	#name : 'AllTestFailTest',
	#superclass : 'MockTest',
	#category : 'TestRunner-Tests-Utilities-Test',
	#package : 'TestRunner-Tests',
	#tag : 'Utilities-Test'
}

{ #category : 'tests' }
AllTestFailTest >> testFail1 [

	self shouldBeIgnored ifTrue: [ ^ self ].
	self assert: false
]

{ #category : 'tests' }
AllTestFailTest >> testFail2 [

	self shouldBeIgnored ifTrue: [ ^ self ].
	self assert: false
]

{ #category : 'tests' }
AllTestFailTest >> testFail3 [

	self shouldBeIgnored ifTrue: [ ^ self ].
	self assert: false
]

{ #category : 'tests' }
AllTestFailTest >> testFail4 [

	self shouldBeIgnored ifTrue: [ ^ self ].
	self assert: false
]
