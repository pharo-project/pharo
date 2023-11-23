"
A BalloonEngineTest is a test class for testing the behavior of BalloonEngine
"
Class {
	#name : 'BalloonEngineTest',
	#superclass : 'TestCase',
	#category : 'FormCanvas-Tests',
	#package : 'FormCanvas-Tests'
}

{ #category : 'tests' }
BalloonEngineTest >> testDefaultBitmapWidth [

	self assert: BalloonEngine defaultBitmapWidth equals: 2048
]

{ #category : 'tests' }
BalloonEngineTest >> testSpan [

	| engine |
	engine := BalloonEngine new.

	Display width > 0
		ifTrue: [ self assert: engine span size equals: Display width + 1 ]
		ifFalse: [ self assert: engine span size equals: BalloonEngine defaultBitmapWidth + 1 ]
]
