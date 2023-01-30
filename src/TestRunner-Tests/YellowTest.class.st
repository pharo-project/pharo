Class {
	#name : #YellowTest,
	#superclass : #MockTest,
	#category : #'TestRunner-Tests-Utilities-Test'
}

{ #category : #tests }
YellowTest >> testFail [

	self shouldBeIgnored ifTrue: [ ^ self ].
	self assert: false
]

{ #category : #tests }
YellowTest >> testPass [
	self assert: true.
]
