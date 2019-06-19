"
I return critiques of all methods visible from my scope
"
Class {
	#name : #ClyAllMethodCritiqueQuery,
	#superclass : #ClyAllCritiqueQuery,
	#category : #'Calypso-SystemPlugins-Critic-Queries'
}

{ #category : #execution }
ClyAllMethodCritiqueQuery >> analyzedObjectsDo: aBlock [
	scope methodsDo: aBlock
]
