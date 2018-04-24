Class {
	#name : #ASTCacheResetTest,
	#superclass : #TestCase,
	#instVars : [
		'cache',
		'counter',
		'link',
		'node'
	],
	#category : #'Reflectivity-Tests'
}

{ #category : #'test material' }
ASTCacheResetTest >> annotatedMethod [
	^ 42
]

{ #category : #'test material' }
ASTCacheResetTest >> increment [
	counter := counter + 1
]

{ #category : #running }
ASTCacheResetTest >> setUp [
	super setUp.
	cache := ASTCache default copy.
	counter := 0.
	link := MetaLink new
		metaObject: self;
		selector: #increment.
	node := (self class >> #annotatedMethod) ast statements last value.
	node link: link
]

{ #category : #running }
ASTCacheResetTest >> tearDown [
	ASTCache default: cache.
	node removeLink: link.
	super tearDown
]

{ #category : #tests }
ASTCacheResetTest >> testCacheResetPreserveLinks [
	self annotatedMethod.
	self assert: counter equals: 1.
	ASTCache reset.
	self assert: (self class >> #annotatedMethod) ast statements last value links anyOne == link.
	self annotatedMethod.
	self assert: counter equals: 2
]
