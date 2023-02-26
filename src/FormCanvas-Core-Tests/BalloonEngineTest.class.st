"
A BalloonEngineTest is a test class for testing the behavior of BalloonEngine
"
Class {
	#name : #BalloonEngineTest,
	#superclass : #TestCase,
	#category : #'FormCanvas-Core-Tests-BalloonEngine'
}

{ #category : #test }
BalloonEngineTest >> testSpan [

	| engine |
	engine := BalloonEngine new.

	Display width > 0
		ifTrue: [ self assert: engine span size equals: Display width ]
		ifFalse: [ self assert: engine span size equals: 2048 ]
]
