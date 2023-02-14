Class {
	#name : #ZnAbstractServerTest,
	#superclass : #TestCase,
	#category : #'Zinc-Tests'
}

{ #category : #testing }
ZnAbstractServerTest class >> isAbstract [

	^ self = ZnAbstractServerTest
]

{ #category : #private }
ZnAbstractServerTest >> port [

	^ 1700 + 32 atRandom
]

{ #category : #private }
ZnAbstractServerTest >> withServerDo: block [

	| server |
	server := ZnServer on: self port.
	[
	server start.
	self assert: server isRunning & server isListening description: ('Failed to start server on port {1}. Is there one already?' format: { server port }).
	block cull: server ] ensure: [ server stop ]
]
