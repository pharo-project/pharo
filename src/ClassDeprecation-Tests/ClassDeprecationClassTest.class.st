"
I am a copy of class ClassDeprecationMetaLinkTest. This comment is copied from there, and might not be entirely accurate

This class contains tests
"
Class {
	#name : #ClassDeprecationClassTest,
	#superclass : #ClassDeprecationTest,
	#category : #'ClassDeprecation-Tests'
}

{ #category : #running }
ClassDeprecationClassTest >> setUp [
	super setUp.
	referencingClass compile: referencingOriginalSource
]

{ #category : #tests }
ClassDeprecationClassTest >> testDeprecationTransformationExpression [
	self
		assert: deprecatedClass deprecationTransformationExpression
		equals: replacementClass name
]

{ #category : #tests }
ClassDeprecationClassTest >> testInstallDeprecationTransformMetaLinks [
	| globalNodes |
	globalNodes := self globalNodes: referencingClass >> #references.
	self deny: globalNodes first hasDeprecationTransformMetaLink.
	self deny: globalNodes second hasDeprecationTransformMetaLink.
	self deny: globalNodes third hasDeprecationTransformMetaLink.
	deprecatedClass installDeprecationTransformMetaLinks.
	globalNodes := self globalNodes: referencingClass >> #references.
	self assert: globalNodes first hasDeprecationTransformMetaLink.
	self assert: globalNodes second hasDeprecationTransformMetaLink.
	self deny: globalNodes third hasDeprecationTransformMetaLink
]

{ #category : #tests }
ClassDeprecationClassTest >> testIsDeprecatedTransformable [
	self deny: referencingClass isDeprecatedTransformable.
	self assert: deprecatedClass isDeprecatedTransformable.
	deprecatedClass class
		compile:
			'isDeprecated
	<transformTo: ''' , replacementClass name
				,
					'''>
	^ false'.
	self deny: deprecatedClass isDeprecatedTransformable.
	deprecatedClass class
		compile:
			'isDeprecated
	^ true'.
	self deny: deprecatedClass isDeprecatedTransformable
]

{ #category : #tests }
ClassDeprecationClassTest >> testUninstallDeprecationTransformMetaLinks [
	| globalNodes |
	deprecatedClass installDeprecationTransformMetaLinks.
	globalNodes := self globalNodes: referencingClass >> #references.
	self assert: globalNodes first hasDeprecationTransformMetaLink.
	self assert: globalNodes second hasDeprecationTransformMetaLink.
	self deny: globalNodes third hasDeprecationTransformMetaLink.
	deprecatedClass uninstallDeprecationTransformMetaLinks.
	globalNodes := self globalNodes: referencingClass >> #references.
	self deny: globalNodes first hasDeprecationTransformMetaLink.
	self deny: globalNodes second hasDeprecationTransformMetaLink.
	self deny: globalNodes third hasDeprecationTransformMetaLink
]
