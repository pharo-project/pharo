"
I model a scope that resolves pharo pseudo-variables such as: `self`, `super` and `thisContext`.

I should by default put at the beginning of the chain when resolving names so these names are resolved with more priority than others.
"
Class {
	#name : 'OCPseudoVariableScope',
	#superclass : 'OCAbstractScope',
	#category : 'OpalCompiler-Core-Semantics',
	#package : 'OpalCompiler-Core',
	#tag : 'Semantics'
}

{ #category : 'lookup' }
OCPseudoVariableScope >> lookupVar: name [

	^ PseudoVariable lookupDictionary
		  at: name asSymbol
		  ifAbsent: [ super lookupVar: name ]
]
