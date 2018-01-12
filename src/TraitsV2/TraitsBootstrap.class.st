"
Utility class with bootstrap scripts
"
Class {
	#name : #TraitsBootstrap,
	#superclass : #Object,
	#category : #'TraitsV2-Bootstrap'
}

{ #category : #bootstrap }
TraitsBootstrap class >> fixSourceCodeOfTraits [
	"This method fix the traits loaded with Hermes"

	| incorrectMethods incorrectClasses |
	incorrectMethods := SystemNavigation new installedMethodsWithIncorrectTrailerKind.
	incorrectClasses := incorrectMethods collect: [ :e | e methodClass ] as:Set.
	incorrectClasses do: [ :e | e rebuildMethodDictionary ].
]
