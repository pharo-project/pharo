Class {
	#name : #FileRegistryTest,
	#superclass : #TestCase,
	#category : #'Files-Tests-Core'
}

{ #category : #tests }
FileRegistryTest >> testFilesAreRegisteredInWeakRegistry [

	|  f |
	f := (File named: 'asd.txt') writeStream.
	[self assert: (File registry keys includes: f)]
		ensure: [ f ifNotNil: [ f close ] ]
]

{ #category : #tests }
FileRegistryTest >> testRegistryShouldBeCleaned [

	| oldSize |
	"We force some garbage collection to avoid noise in the test"
	3 timesRepeat: [Smalltalk garbageCollect].
	oldSize := File registry size.

	(File named: 'asd.txt') writeStream.
	3 timesRepeat: [Smalltalk garbageCollect].
	
	self assert: oldSize equals: File registry size.
]
