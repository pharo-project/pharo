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
	#name : #OCExtraBindingScope,
	#superclass : #OCAbstractScope,
	#instVars : [
		'bindings'
	],
	#category : #'OpalCompiler-Core-Semantics'
}

{ #category : #accessing }
OCExtraBindingScope >> bindings [
	^ bindings
]

{ #category : #accessing }
OCExtraBindingScope >> bindings: anObject [
	bindings := anObject
]

{ #category : #lookup }
OCExtraBindingScope >> lookupVar: name declare: aBoolean [
	^(bindings bindingOf: name asSymbol)
		ifNotNil: [ :var | var ]
		ifNil: [ outerScope lookupVar: name declare: aBoolean]
]
