Class {
	#name : #TKTMemoryLeakTest,
	#superclass : #TKTTestCase,
	#category : #'TaskIt-Tests-Kernel'
}

{ #category : #helpers }
TKTMemoryLeakTest >> trackInstancesOf: aClass during: aBlock [

	| before |
	before := aClass allInstances size.

	aBlock value.

	self assertWithGarbageCollect: [ before >= aClass allInstances size ].
]
