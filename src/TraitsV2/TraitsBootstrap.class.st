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

	| incorrectMethods |
	incorrectMethods := SystemNavigation new installedMethodsWithIncorrectTrailerKind collect: [ :e | e compiledMethod ].
	incorrectMethods
		do: [ :aMethod | 
			| aClass defining |
			aClass := aMethod methodClass.
			defining := aClass traitComposition traitDefining: aMethod selector ifNone: [ aMethod traitSource ].
			defining installSelector: aMethod selector into: aClass replacing: true ]
]
