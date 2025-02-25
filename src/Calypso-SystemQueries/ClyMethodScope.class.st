"
I am a scope of methods.

Currently I do not provide any information except methods itself
"
Class {
	#name : 'ClyMethodScope',
	#superclass : 'ClyTypedScope',
	#category : 'Calypso-SystemQueries-Scopes',
	#package : 'Calypso-SystemQueries',
	#tag : 'Scopes'
}

{ #category : 'accessing' }
ClyMethodScope class >> defaultName [
	^'methods'
]

{ #category : 'queries' }
ClyMethodScope >> methodsDo: aBlock [
	basisObjects do: aBlock
]

{ #category : 'initialization' }
ClyMethodScope >> prepareBasisFrom: aCollection [
	^aCollection asIdentitySet
]
