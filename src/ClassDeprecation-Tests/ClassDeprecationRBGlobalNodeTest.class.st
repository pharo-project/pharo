"
I am a copy of class ClassDeprecationClassTest. This comment is copied from there, and might not be entirely accurate

I am a copy of class ClassDeprecationMetaLinkTest. This comment is copied from there, and might not be entirely accurate

This class contains tests
"
Class {
	#name : #ClassDeprecationRBGlobalNodeTest,
	#superclass : #ClassDeprecationTest,
	#category : #'ClassDeprecation-Tests'
}

{ #category : #accessing }
ClassDeprecationRBGlobalNodeTest >> globalNodes [
	^ self globalNodes: referencingClass >> #references
]

{ #category : #running }
ClassDeprecationRBGlobalNodeTest >> setUp [
	super setUp.
	referencingClass compile: referencingOriginalSource
]

{ #category : #tests }
ClassDeprecationRBGlobalNodeTest >> testDeprecationTransform [
	| globalNode result |
	self
		should: [ self globalNodes third deprecationTransform ]
		raise: AssertionFailure.
	globalNode := self globalNodes first.
	result := globalNode deprecationTransform.
	globalNode methodNode doSemanticAnalysis.
	self deny: globalNode equals: result.
	globalNode := self globalNodes first.
	self
		assert: globalNode methodNode formattedCode
		equals:
			'references
	^ {ClassDeprecationForTestReplacement.
	1.
	self class.
	ClassDeprecationForTestDeprecated.
	ClassDeprecationForTestReferencing}'.
	self assert: result isVariable.
	self assert: result isGlobal.
	self assert: result binding isGlobalClassNameBinding.
	self assert: result binding value equals: replacementClass
]

{ #category : #tests }
ClassDeprecationRBGlobalNodeTest >> testDeprecationTransformMetaLink [
	| globalNodes |
	globalNodes := self globalNodes.
	self
		should: [ globalNodes first deprecationTransformMetaLink ]
		raise: NotFound.
	self
		should: [ globalNodes second deprecationTransformMetaLink ]
		raise: NotFound.
	self
		should: [ globalNodes third deprecationTransformMetaLink ]
		raise: NotFound.
	ClassDeprecationMetaLink enable.
	globalNodes := self globalNodes.
	self
		assert:
			(globalNodes first deprecationTransformMetaLink
				isKindOf: ClassDeprecationMetaLink).
	self
		assert:
			(globalNodes second deprecationTransformMetaLink
				isKindOf: ClassDeprecationMetaLink).
	self
		should: [ globalNodes third deprecationTransformMetaLink ]
		raise: NotFound
]

{ #category : #tests }
ClassDeprecationRBGlobalNodeTest >> testHasDeprecationTransformMetaLink [
	| globalNodes |
	globalNodes := self globalNodes.
	self deny: globalNodes first hasDeprecationTransformMetaLink.
	self deny: globalNodes second hasDeprecationTransformMetaLink.
	self deny: globalNodes third hasDeprecationTransformMetaLink.
	ClassDeprecationMetaLink enable.
	globalNodes := self globalNodes.
	self assert: globalNodes first hasDeprecationTransformMetaLink.
	self assert: globalNodes second hasDeprecationTransformMetaLink.
	self deny: globalNodes third hasDeprecationTransformMetaLink
]

{ #category : #tests }
ClassDeprecationRBGlobalNodeTest >> testInstallDeprecationTransformMetaLink [
	| globalNode |
	globalNode := self globalNodes first.
	self assert: globalNode links size equals: 0.
	globalNode installDeprecationTransformMetaLink.
	self assert: globalNode links size equals: 1.
	self
		assert: (globalNode links first isKindOf: ClassDeprecationMetaLink).
	globalNode installDeprecationTransformMetaLink.
	self assert: globalNode links size equals: 1.
	self
		assert: (globalNode links first isKindOf: ClassDeprecationMetaLink)
]

{ #category : #tests }
ClassDeprecationRBGlobalNodeTest >> testIsDeprecatedTransformable [
	| globalNodes |
	globalNodes := self globalNodes.
	self assert: globalNodes first isDeprecatedTransformable.
	self assert: globalNodes second isDeprecatedTransformable.
	self deny: globalNodes third isDeprecatedTransformable
]

{ #category : #tests }
ClassDeprecationRBGlobalNodeTest >> testSetUp [
	| globalNodes |
	globalNodes := self globalNodes.
	self assert: globalNodes size equals: 3
]

{ #category : #tests }
ClassDeprecationRBGlobalNodeTest >> testUninstallDeprecationTransformMetaLink [
	| globalNode |
	globalNode := self globalNodes first.
	self assert: globalNode links size equals: 0.
	globalNode uninstallDeprecationTransformMetaLink.
	self assert: globalNode links size equals: 0.
	globalNode installDeprecationTransformMetaLink.
	self assert: globalNode links size equals: 1.
	globalNode uninstallDeprecationTransformMetaLink.
	self assert: globalNode links size equals: 0
]
