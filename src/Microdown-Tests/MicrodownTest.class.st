Class {
	#name : #MicrodownTest,
	#superclass : #TestCase,
	#category : #'Microdown-Tests-Parser'
}

{ #category : #tests }
MicrodownTest >> testResolverRootIsCorrectlyPassedToResolver [

	| mic root |
	mic := Microdown new.  
	root := 'http://www.pharo.org' asUrl.
	mic resolverRoot: root.
	self assert: mic resolverRoot equals: mic resolver resolutionSource
]
