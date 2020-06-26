Class {
	#name : #TKTMemoryLeakTests,
	#superclass : #TKTTestCase,
	#category : #'TaskIt-Tests-Kernel'
}

{ #category : #helpers }
TKTMemoryLeakTests >> trackInstancesOf: aClass during: aBlock [

	| before |
	
	self garbageCollectAndWait .
	
	before := aClass allInstances size.
	
	aBlock value.
	
	self garbageCollectAndWait .
	
	self assert: before equals: aClass allInstances size.
]
