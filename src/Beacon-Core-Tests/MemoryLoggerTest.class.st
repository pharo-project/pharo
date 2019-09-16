"
A MemoryLoggerTest is xxxxxxxxx.

"
Class {
	#name : #MemoryLoggerTest,
	#superclass : #TestCase,
	#classInstVars : [
		'previousRecordings'
	],
	#category : #'Beacon-Core-Tests-Tests'
}

{ #category : #testing }
MemoryLoggerTest >> testRecordOneSignal [
	| logger |
	logger := MemoryLogger new runDuring: [ StringSignal emit: 'test' ].
	self assert: logger recordings size equals: 1.
	self assert: logger recordings first message equals: 'test'
]

{ #category : #testing }
MemoryLoggerTest >> testRecordTwoSignalsInTheRightOrder [
	| logger |
	logger := MemoryLogger new 
					runDuring: [ 
						StringSignal emit: 'test1'.
						StringSignal emit: 'test2' ].
	self assert: logger recordings size equals: 2.
	self assert: logger recordings first message equals: 'test1'.
	self assert: logger recordings last message equals: 'test2'
]

{ #category : #testing }
MemoryLoggerTest >> testRunDuringStopsAtTheEnd [
	| logger |
	logger := MemoryLogger new.
	logger runDuring: [ 
		self assert: logger isRunning ].
	self assert: logger isRunning not
]
