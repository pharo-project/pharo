"
Bindings normally only come from the scope hierarchy that the variable is in: method, instance, class, requestor (if any). 

The ExtraBinding Scope allows to define bindings via a compiler API.

	result := Smalltalk compiler
		bindings: {(#a -> 3)} asDictionary;
		evaluate: '1+a'.
		
these binding shadow all other bindings, even globals:

result := Smalltalk compiler
		bindings: {(#Object -> 3)} asDictionary;
		evaluate: 'Object + 1'.
		
Shadowed global variables can be assigned to even if they referene a class.
"
Class {
	#name : 'OCExtraBindingScope',
	#superclass : 'OCAbstractScope',
	#instVars : [
		'bindings'
	],
	#category : 'OpalCompiler-Core-Semantics',
	#package : 'OpalCompiler-Core',
	#tag : 'Semantics'
}

{ #category : 'temp vars' }
OCExtraBindingScope >> allTemps [

	^ #(  )
]

{ #category : 'accessing' }
OCExtraBindingScope >> bindings [

	^ bindings
]

{ #category : 'accessing' }
OCExtraBindingScope >> bindings: arg1 [

	bindings := arg1
]

{ #category : 'lookup' }
OCExtraBindingScope >> lookupVar: arg1 [

	| tmp1 |
	^ (tmp1 := bindings bindingOf: arg1 asSymbol)
		  ifNil: [ super lookupVar: arg1 ]
		  ifNotNil: [ tmp1 ]
]
