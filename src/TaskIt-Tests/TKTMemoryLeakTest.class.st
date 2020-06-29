Class {
	#name : #TKTMemoryLeakTest,
	#superclass : #TKTTestCase,
	#category : #'TaskIt-Tests-Kernel'
}

{ #category : #helpers }
TKTMemoryLeakTest >> trackInstancesOf: aClass during: aBlock [

	| before |
	
	self garbageCollectAndWait .
	
	before := aClass allInstances size.
	
	aBlock value.
	
	self garbageCollectAndWait .
	
	self assert: before >= aClass allInstances size.
]
