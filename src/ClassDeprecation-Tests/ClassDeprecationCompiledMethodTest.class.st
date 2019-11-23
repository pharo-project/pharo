"
I am a copy of class ClassDeprecationClassTest. This comment is copied from there, and might not be entirely accurate

I am a copy of class ClassDeprecationMetaLinkTest. This comment is copied from there, and might not be entirely accurate

This class contains tests
"
Class {
	#name : #ClassDeprecationCompiledMethodTest,
	#superclass : #ClassDeprecationTest,
	#category : #'ClassDeprecation-Tests'
}

{ #category : #running }
ClassDeprecationCompiledMethodTest >> setUp [
	super setUp.
	referencingClass compile: referencingOriginalSource
]

{ #category : #tests }
ClassDeprecationCompiledMethodTest >> testHasDeprecationTransformableReferences [
	self
		assert:
			(referencingClass >> #references)
				hasDeprecationTransformableReferences.
	self
		deny:
			(deprecatedClass class >> #isDeprecated)
				hasDeprecationTransformableReferences
]

{ #category : #tests }
ClassDeprecationCompiledMethodTest >> testInstallDeprecationTransformMetaLinks [
	| globalNodes |
	globalNodes := self globalNodes: referencingClass >> #references.
	self deny: globalNodes first hasDeprecationTransformMetaLink.
	self deny: globalNodes second hasDeprecationTransformMetaLink.
	self deny: globalNodes third hasDeprecationTransformMetaLink.
	(referencingClass >> #references)
		installDeprecationTransformMetaLinks.
	globalNodes := self globalNodes: referencingClass >> #references.
	self assert: globalNodes first hasDeprecationTransformMetaLink.
	self assert: globalNodes second hasDeprecationTransformMetaLink.
	self deny: globalNodes third hasDeprecationTransformMetaLink.
	globalNodes := self
		globalNodes: deprecatedClass class >> #isDeprecated.
	self assert: globalNodes size equals: 0.
	(deprecatedClass class >> #isDeprecated)
		installDeprecationTransformMetaLinks.
	globalNodes := self
		globalNodes: deprecatedClass class >> #isDeprecated.
	self assert: globalNodes size equals: 0
]

{ #category : #tests }
ClassDeprecationCompiledMethodTest >> testIsDeprecationTransformableMethod [
	self
		deny: (referencingClass >> #references) isDeprecationTransformableMethod.
	self
		assert:
			(deprecatedClass class >> #isDeprecated)
				isDeprecationTransformableMethod
]

{ #category : #tests }
ClassDeprecationCompiledMethodTest >> testUninstallDeprecationTransformMetaLinks [
	| globalNodes |
	(referencingClass >> #references)
		installDeprecationTransformMetaLinks.
	globalNodes := self globalNodes: referencingClass >> #references.
	self assert: globalNodes first hasDeprecationTransformMetaLink.
	self assert: globalNodes second hasDeprecationTransformMetaLink.
	self deny: globalNodes third hasDeprecationTransformMetaLink.
	(referencingClass >> #references)
		uninstallDeprecationTransformMetaLinks.
	globalNodes := self globalNodes: referencingClass >> #references.
	self deny: globalNodes first hasDeprecationTransformMetaLink.
	self deny: globalNodes second hasDeprecationTransformMetaLink.
	self deny: globalNodes third hasDeprecationTransformMetaLink.
	globalNodes := self
		globalNodes: deprecatedClass class >> #isDeprecated.
	self assert: globalNodes size equals: 0.
	(deprecatedClass class >> #isDeprecated)
		uninstallDeprecationTransformMetaLinks.
	globalNodes := self
		globalNodes: deprecatedClass class >> #isDeprecated.
	self assert: globalNodes size equals: 0
]
