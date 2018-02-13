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
	"Globals are not shadowed"
	name first isUppercase ifTrue: [  | found |
		found := outerScope lookupVar: name.
		found ifNotNil: [ ^found ]].

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
