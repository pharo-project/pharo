"
I return all critiques directly from basis objects of my scope.
So I am a kind of universal query because I do not restrict scopes for information which they provide.
I just ask scope basis objects for critiques
"
Class {
	#name : #ClyAllBasisCritiqueQuery,
	#superclass : #ClyAllCritiqueQuery,
	#category : #'Calypso-SystemPlugins-Critic-Queries'
}

{ #category : #execution }
ClyAllBasisCritiqueQuery >> analyzedObjectsDo: aBlock [
	scope basisObjects do: aBlock
]

{ #category : #printing }
ClyAllBasisCritiqueQuery >> description [

	^'critiques of scope basis'
]
