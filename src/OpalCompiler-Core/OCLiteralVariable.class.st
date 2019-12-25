"
I am a global, pool, or class variable.
"
Class {
	#name : #OCLiteralVariable,
	#superclass : #OCAbstractVariable,
	#instVars : [
		'assoc'
	],
	#category : #'OpalCompiler-Core-Semantics'
}

{ #category : #accessing }
OCLiteralVariable class >> semanticNodeClass [

	^RBGlobalNode
]

{ #category : #accessing }
OCLiteralVariable >> assoc [
	^ assoc
]

{ #category : #initializing }
OCLiteralVariable >> assoc: anAssociation [

	assoc := anAssociation
]

{ #category : #emitting }
OCLiteralVariable >> emitStore: methodBuilder [

	 assoc emitStore: methodBuilder


]

{ #category : #emitting }
OCLiteralVariable >> emitValue: methodBuilder [
	assoc emitValue: methodBuilder
]

{ #category : #testing }
OCLiteralVariable >> isClassVariable [
	^ assoc isClassVariable
]

{ #category : #testing }
OCLiteralVariable >> isFromSharedPool [
	| sharedPools |
	sharedPools := scope getClass instanceSide sharedPools.
	^ sharedPools 
		detect: [ :pool | pool classVarNames includes: assoc key ]
		ifFound: [ true ]
		ifNone: [ false ]
]

{ #category : #testing }
OCLiteralVariable >> isGlobal [

	^ true
]

{ #category : #testing }
OCLiteralVariable >> isGlobalClassNameBinding [
	^ (self value isClass or: [ self value isTrait ])
		and: [ self variable key == self value name ]
]

{ #category : #testing }
OCLiteralVariable >> isGlobalVariable [
	^ assoc isGlobalVariable
]

{ #category : #testing }
OCLiteralVariable >> isLiteralVariable [
	^true
]

{ #category : #testing }
OCLiteralVariable >> isWritable [
	"Literal variables are writable, if they aren't global bindings for class names, like
 #Object -> Object "

	^ (self isGlobalVariable and: [ self isGlobalClassNameBinding ]) not
]

{ #category : #accessing }
OCLiteralVariable >> name [

	^ assoc name
]

{ #category : #reading }
OCLiteralVariable >> read [
	^ assoc read 
]

{ #category : #accessing }
OCLiteralVariable >> value [
	^assoc value
]

{ #category : #accessing }
OCLiteralVariable >> variable [
	^assoc
]
