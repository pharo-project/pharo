Class {
	#name : 'StackSignalTest',
	#superclass : 'TestCase',
	#category : 'Beacon-Core-Tests-Tests',
	#package : 'Beacon-Core-Tests',
	#tag : 'Tests'
}

{ #category : 'tests' }
StackSignalTest >> testCaptureStackOfCurrentExecution [

	| m |
	m := MemoryLogger new.
	m	runDuring: [ 
			StackSignal emit ].
	
	self assert: m entries first stack selector equals: thisContext selector

]
