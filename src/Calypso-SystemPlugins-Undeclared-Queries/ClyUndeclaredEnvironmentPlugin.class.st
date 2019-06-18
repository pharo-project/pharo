"
I am special plugin which decorate methods which use undeclared variables
Also I provide ""undeclared"" method group to see all such methods together
"
Class {
	#name : #ClyUndeclaredEnvironmentPlugin,
	#superclass : #ClySystemEnvironmentPlugin,
	#category : #'Calypso-SystemPlugins-Undeclared-Queries'
}

{ #category : #'method groups' }
ClyUndeclaredEnvironmentPlugin >> collectMethodGroupProviders [
	^{ClyUndeclaredMethodGroupProvider new}
]

{ #category : #'item decoration' }
ClyUndeclaredEnvironmentPlugin >> decorateBrowserItem: anItem ofMethod: aMethod [

	aMethod usesUndeclares ifTrue: [ 
		anItem markWith: ClyUndeclaresUserTag]
]
