"
A BalloonEngineTest is a test class for testing the behavior of BalloonEngine
"
Class {
	#name : #BalloonEngineTest,
	#superclass : #TestCase,
	#category : #'FormCanvas-Tests'
}

{ #category : #tests }
BalloonEngineTest >> testDefaultBitmapWidth [

	self assert: BalloonEngine defaultBitmapWidth equals: 2048
]

{ #category : #tests }
BalloonEngineTest >> testSpan [

	| engine |
	engine := BalloonEngine new.

	Display width > 0
		ifTrue: [ self assert: engine span size equals: Display width ]
		ifFalse: [ self assert: engine span size equals: BalloonEngine defaultBitmapWidth ]
]
