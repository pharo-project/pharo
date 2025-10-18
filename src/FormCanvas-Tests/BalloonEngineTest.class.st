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
