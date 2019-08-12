"
I am special plugin which analyzes deprecated methods/classes and mark them with deprecation tag.
Also I provide deprecated method group to see all deprecated methods together
"
Class {
	#name : #ClyDeprecationEnvironmentPlugin,
	#superclass : #ClySystemEnvironmentPlugin,
	#category : #'Calypso-SystemPlugins-Deprecation-Queries'
}

{ #category : #'method groups' }
ClyDeprecationEnvironmentPlugin >> collectMethodGroupProviders [
	^{ClyDeprecatedMethodGroupProvider new}
]

{ #category : #'item decoration' }
ClyDeprecationEnvironmentPlugin >> decorateBrowserItem: anItem ofClass: aClass [
	aClass isDeprecated ifTrue: [ anItem markWith: ClyDeprecatedItemTag ]
]

{ #category : #'item decoration' }
ClyDeprecationEnvironmentPlugin >> decorateBrowserItem: anItem ofMethod: aMethod [

	aMethod isDeprecated ifTrue: [ 
		anItem markWith: ClyDeprecatedItemTag]
]

{ #category : #'item decoration' }
ClyDeprecationEnvironmentPlugin >> decorateBrowserItem: anItem ofPackage: aPackage [
	aPackage isDeprecated ifTrue: [ anItem markWith: ClyDeprecatedItemTag ]
]
