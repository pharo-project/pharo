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
OCExtraBindingScope >> findVariable: aBlock ifNone: aNotFound [
	outerScope ifNotNil: [
		^  outerScope findVariable: aBlock ifNone: aNotFound ]
]

{ #category : #lookup }
OCExtraBindingScope >> lookupVar: name [
	name = 'self' ifTrue: [ ^ outerScope lookupVar: name ].
	name = 'super' ifTrue: [ ^ outerScope lookupVar: name ].
	"the requestors #bindingOf may create a binding for not yet existing variables"
	(bindings bindingOf: name asSymbol)
		ifNotNil: [ :assoc | 
			^ OCLiteralVariable new
				assoc: assoc;
				scope: self;
				yourself ].
	^ super lookupVar: name 
]

{ #category : #lookup }
OCExtraBindingScope >> lookupVar: name inContext: context [

	name = 'self' ifTrue: [  ^outerScope lookupVar: name inContext: context outerContext].
	name = 'super' ifTrue: [  ^outerScope lookupVar: name inContext: context outerContext].
	"Globals are not shadowed"
	name first isUppercase ifTrue: [  | found |
		found := outerScope outerScope lookupVar: name inContext: context outerContext.
		found ifNotNil: [ ^found ]].
	
	(bindings bindingOf: name asSymbol) ifNotNil: [:assoc | 
		^ OCLiteralVariable new assoc: assoc; scope: self; yourself].
	^ super lookupVar: name inContext: context outerContext.
]

{ #category : #'instance creation' }
OCExtraBindingScope >> newMethodScope [
	 
	^ OCMethodScope new outerScope: (self outerScope: outerScope instanceScope) 
]

{ #category : #'instance creation' }
OCExtraBindingScope >> variableNamed: name ifAbsent: aBlock [

	name = 'self' ifTrue: [  ^outerScope lookupVar: name].
	name = 'super' ifTrue: [  ^outerScope lookupVar: name].
	name first isUppercase ifTrue: [  | found |
		found := outerScope lookupVar: name.
		found ifNotNil: [ ^found ]].
	
	(bindings bindingOf: name asSymbol) ifNotNil: [:assoc | 
		^ OCLiteralVariable new assoc: assoc; scope: self; yourself].

	^ aBlock value
]
