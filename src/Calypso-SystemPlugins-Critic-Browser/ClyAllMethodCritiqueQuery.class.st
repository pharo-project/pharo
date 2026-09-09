"
I return critiques of all methods visible from my scope
"
Class {
	#name : 'ClyAllMethodCritiqueQuery',
	#superclass : 'ClyAllCritiqueQuery',
	#category : 'Calypso-SystemPlugins-Critic-Browser',
	#package : 'Calypso-SystemPlugins-Critic-Browser'
}

{ #category : 'execution' }
ClyAllMethodCritiqueQuery >> analyzedObjectsDo: aBlock [
	scope methodsDo: aBlock
]
