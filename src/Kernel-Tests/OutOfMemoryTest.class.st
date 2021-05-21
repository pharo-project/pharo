Class {
	#name : #OutOfMemoryTest,
	#superclass : #TestCase,
	#instVars : [
		'oldValue'
	],
	#category : #'Kernel-Tests-Exception'
}

{ #category : #running }
OutOfMemoryTest >> setUp [ 

	super setUp.
	oldValue := Smalltalk vm maxOldSpaceSize: 2 * 1024 *1024 * 1024.
]

{ #category : #running }
OutOfMemoryTest >> tearDown [ 

	Smalltalk vm maxOldSpaceSize: oldValue.
	super tearDown.
]

{ #category : #tests }
OutOfMemoryTest >> testErrorProducedByAllocatingInTheImage [

	"I generate an error when executing the allocation."
	self should: [ ByteArray new: 100 * 1000 * 1000000 ] raise: OutOfMemory
]

{ #category : #tests }
OutOfMemoryTest >> testSignalOfTheLowSpaceFromTheVM [

	| s |
	s := Semaphore new.

	"I emulate the execution when the signal is send by the VM"

	self should: [
		Smalltalk specialObjectsArray at: 23 put: Processor activeProcess.
		Smalltalk signalLowSpace.
		s wait] raise: OutOfMemory
]
