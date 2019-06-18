"
I represent class scope of all users of my basis traits.
I see methods of all classes and traits which use my basis traits 
"
Class {
	#name : #ClyTraitUserScope,
	#superclass : #ClySingleClassRelationScope,
	#category : #'Calypso-SystemPlugins-Traits-Queries'
}

{ #category : #accessing }
ClyTraitUserScope class >> classesRelatedTo: aTrait do: aBlock [
	aTrait users do: aBlock
]

{ #category : #accessing }
ClyTraitUserScope class >> defaultName [
	^'trait users'
]
