Class {
	#name : 'ReferenceFinderTest',
	#superclass : 'TestCase',
	#category : 'ReferenceFinder-Core-Base',
	#package : 'ReferenceFinder-Core',
	#tag : 'Base'
}

{ #category : 'tests' }
ReferenceFinderTest >> testEphemeronsAreIgnored [
	| obj dict |
	obj := Object new.
	dict := WeakKeyDictionary new.
	dict at: obj put: 1.
	self assert: (ReferenceFinder findPathTo: obj from: dict) isNil
]
