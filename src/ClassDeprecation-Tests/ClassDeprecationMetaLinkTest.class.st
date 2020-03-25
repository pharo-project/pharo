Class {
	#name : #ClassDeprecationMetaLinkTest,
	#superclass : #ClassDeprecationTest,
	#instVars : [
		'referencingIntermediateSource',
		'metaLink'
	],
	#category : #'ClassDeprecation-Tests'
}

{ #category : #running }
ClassDeprecationMetaLinkTest >> setUp [
	super setUp.
	metaLink := ClassDeprecationMetaLink new.
	referencingIntermediateSource := 'references
	^ {ClassDeprecationForTestReplacement.
	1.
	self class.
	ClassDeprecationForTestDeprecated.
	ClassDeprecationForTestReferencing}'..
	referencingClass compile: referencingOriginalSource
]

{ #category : #tests }
ClassDeprecationMetaLinkTest >> testDeprecationTransform [
	| globalNode |
	globalNode := (self globalNodes: referencingClass >> #references)
		first.
	metaLink deprecationTransform: globalNode.
	self
		assert: (referencingClass >> #references) sourceCode
		equals: referencingIntermediateSource.
	globalNode := (self globalNodes: referencingClass >> #references)
		third.
	self
		should: [ metaLink deprecationTransform: globalNode ]
		raise: AssertionFailure.
	self
		assert: (referencingClass >> #references) sourceCode
		equals: referencingIntermediateSource
]

{ #category : #tests }
ClassDeprecationMetaLinkTest >> testSetUp [
	self assert: metaLink control equals: #before.
	self assert: metaLink metaObject equals: metaLink.
	self assert: metaLink selector equals: #deprecationTransform:
]
