"
I am a copy of class ClassDeprecationMetaLinkTest. This comment is copied from there, and might not be entirely accurate

This class contains tests
"
Class {
	#name : #ClassDeprecationMetaLinkInstallerTest,
	#superclass : #ClassDeprecationTest,
	#category : #'ClassDeprecation-Tests'
}

{ #category : #running }
ClassDeprecationMetaLinkInstallerTest >> setUp [
	super setUp.
	referencingClass compile: referencingOriginalSource
]

{ #category : #tests }
ClassDeprecationMetaLinkInstallerTest >> testVisit [
	| globalNodes |
	globalNodes := self globalNodes: referencingClass >> #references.
	self deny: globalNodes first hasDeprecationTransformMetaLink.
	self deny: globalNodes second hasDeprecationTransformMetaLink.
	self deny: globalNodes third hasDeprecationTransformMetaLink.
	ClassDeprecationMetaLinkInstaller new
		visitNode: (referencingClass >> #references) ast.
	globalNodes := self globalNodes: referencingClass >> #references.
	self assert: globalNodes first hasDeprecationTransformMetaLink.
	self assert: globalNodes second hasDeprecationTransformMetaLink.
	self deny: globalNodes third hasDeprecationTransformMetaLink.
	globalNodes := self
		globalNodes: deprecatedClass class >> #isDeprecated.
	self assert: globalNodes size equals: 0.
	ClassDeprecationMetaLinkInstaller new
		visitNode: (deprecatedClass class >> #isDeprecated) ast.
	globalNodes := self
		globalNodes: deprecatedClass class >> #isDeprecated.
	self assert: globalNodes size equals: 0
]
