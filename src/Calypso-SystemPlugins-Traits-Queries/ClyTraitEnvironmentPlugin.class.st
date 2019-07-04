"
I plug environment with information about traits.

I decorate classes that they are traits, or users of traits.
I decorate methods that they are defined by trait.

And I extend method visibility of inherited method group which provide extra checkbox in the browser to enable/disable visibility of trait methods of selected classes
"
Class {
	#name : #ClyTraitEnvironmentPlugin,
	#superclass : #ClySystemEnvironmentPlugin,
	#category : #'Calypso-SystemPlugins-Traits-Queries'
}

{ #category : #'item decoration' }
ClyTraitEnvironmentPlugin >> decorateBrowserItem: anItem ofClass: aClass [

	aClass isTrait ifTrue: [ anItem markWith: ClyTraitTag ].
	aClass traits ifNotEmpty: [ anItem markWith: ClyTraitUserTag ]
]

{ #category : #'item decoration' }
ClyTraitEnvironmentPlugin >> decorateBrowserItem: anItem ofMethod: aMethod [

	aMethod origin isTrait ifTrue: [ 
		anItem systemDefinition definingClassItem markWith: ClyTraitTag ]
]
