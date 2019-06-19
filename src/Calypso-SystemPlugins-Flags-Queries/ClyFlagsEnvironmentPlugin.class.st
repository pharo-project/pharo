"
I am special plugin which mark methods which have any flag.
Also I provide flags method group to see all such methods together
"
Class {
	#name : #ClyFlagsEnvironmentPlugin,
	#superclass : #ClySystemEnvironmentPlugin,
	#category : #'Calypso-SystemPlugins-Flags-Queries'
}

{ #category : #'method groups' }
ClyFlagsEnvironmentPlugin >> collectMethodGroupProviders [ 
	^{ClyFlagMethodGroupProvider new}
]

{ #category : #'item decoration' }
ClyFlagsEnvironmentPlugin >> decorateBrowserItem: anItem ofMethod: aMethod [

	(aMethod sendsAnySelectorOf: #(flag:)) ifTrue: [ 
		anItem markWith: ClyMethodFlag]
]
