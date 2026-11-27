"
I am an `OSSDL2Driver` for tests: I do not start SDL, and my event loop only waits until it is stopped. Tests give me an `OSSDL2EventLoopRunner` of their own (`eventLoopRunner:`), so they check how a driver starts, keeps and replaces its loop without touching the image's loop, in a headless image too.

## Invariants and Assumptions

- I am never chosen as the image's driver: `isSuitable` answers false.
"
Class {
	#name : 'OSSDL2DriverMock',
	#superclass : 'OSSDL2Driver',
	#category : 'OSWindow-Tests-Mocks',
	#package : 'OSWindow-Tests',
	#tag : 'Mocks'
}

{ #category : 'testing' }
OSSDL2DriverMock class >> isSuitable [

	^ false
]

{ #category : 'events - processing' }
OSSDL2DriverMock >> eventLoop [
	"Wait until stopped, without polling SDL"

	Semaphore new wait
]

{ #category : 'initialization' }
OSSDL2DriverMock >> initialize [
	"Do not start SDL"

	
]
