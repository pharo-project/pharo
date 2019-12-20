"
I am a copy of class ClassDeprecationMetaLinkInstallerTest. This comment is copied from there, and might not be entirely accurate

I am a copy of class ClassDeprecationMetaLinkTest. This comment is copied from there, and might not be entirely accurate

This class contains tests
"
Class {
	#name : #ClassDeprecationMetaLinkUninstallerTest,
	#superclass : #ClassDeprecationTest,
	#category : #'ClassDeprecation-Tests'
}

{ #category : #running }
ClassDeprecationMetaLinkUninstallerTest >> setUp [
	super setUp.
	referencingClass compile: referencingOriginalSource
]

{ #category : #tests }
ClassDeprecationMetaLinkUninstallerTest >> testVisit [
	| globalNodes |
	ClassDeprecationMetaLinkInstaller new
		visitNode: (referencingClass >> #references) ast.
	globalNodes := self globalNodes: referencingClass >> #references.
	self assert: globalNodes first hasDeprecationTransformMetaLink.
	self assert: globalNodes second hasDeprecationTransformMetaLink.
	self deny: globalNodes third hasDeprecationTransformMetaLink.
	ClassDeprecationMetaLinkUninstaller new
		visitNode: (referencingClass >> #references) ast.
	globalNodes := self globalNodes: referencingClass >> #references.
	self deny: globalNodes first hasDeprecationTransformMetaLink.
	self deny: globalNodes second hasDeprecationTransformMetaLink.
	self deny: globalNodes third hasDeprecationTransformMetaLink.
	globalNodes := self
		globalNodes: deprecatedClass class >> #isDeprecated.
	self assert: globalNodes size equals: 0.
	ClassDeprecationMetaLinkUninstaller new
		visitNode: (deprecatedClass class >> #isDeprecated) ast.
	globalNodes := self
		globalNodes: deprecatedClass class >> #isDeprecated.
	self assert: globalNodes size equals: 0
]
