Class {
	#name : 'OCCacheResetTest',
	#superclass : 'TestCase',
	#instVars : [
		'cache',
		'counter',
		'link',
		'node'
	],
	#category : 'Reflectivity-Tests-Base',
	#package : 'Reflectivity-Tests',
	#tag : 'Base'
}

{ #category : 'tests - material' }
OCCacheResetTest >> annotatedMethod [
	^ 42
]

{ #category : 'tests - material' }
OCCacheResetTest >> increment [
	counter := counter + 1
]

{ #category : 'running' }
OCCacheResetTest >> setUp [
	super setUp.
	cache := OCASTCache default copy.
	counter := 0.
	link := MetaLink new
		metaObject: self;
		selector: #increment.
	node := (self class >> #annotatedMethod) ast statements last value.
	node link: link
]

{ #category : 'running' }
OCCacheResetTest >> tearDown [
	OCASTCache default: cache.
	node removeLink: link.
	super tearDown
]

{ #category : 'tests' }
OCCacheResetTest >> testCacheResetPreserveLinks [
	self annotatedMethod.
	self assert: counter equals: 1.
	OCASTCache reset.
	self assert: (self class >> #annotatedMethod) ast statements last value links anyOne identicalTo: link.
	self annotatedMethod.
	self assert: counter equals: 2
]
