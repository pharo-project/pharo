"
I am a global, pool, or class variable.
"
Class {
	#name : #OCLiteralVariable,
	#superclass : #OCAbstractVariable,
	#instVars : [
		'variable'
	],
	#category : #'Deprecated90-OpalCompiler-Core'
}

{ #category : #testing }
OCLiteralVariable class >> isDeprecated [
	"Use LiteralVariable directly instead"
	^true
]

{ #category : #accessing }
OCLiteralVariable class >> semanticNodeClass [

	^RBGlobalNode
]

{ #category : #emitting }
OCLiteralVariable >> emitStore: methodBuilder [

	 variable emitStore: methodBuilder


]

{ #category : #emitting }
OCLiteralVariable >> emitValue: methodBuilder [
	variable emitValue: methodBuilder
]

{ #category : #testing }
OCLiteralVariable >> isClassVariable [
	^ variable isClassVariable
]

{ #category : #testing }
OCLiteralVariable >> isFromSharedPool [
	| sharedPools |
	sharedPools := scope getClass instanceSide sharedPools.
	^ sharedPools 
		detect: [ :pool | pool classVarNames includes: variable key ]
		ifFound: [ true ]
		ifNone: [ false ]
]

{ #category : #testing }
OCLiteralVariable >> isGlobalClassNameBinding [
	^ (self value isClass or: [ self value isTrait ])
		and: [ self variable key == self value name ]
]

{ #category : #testing }
OCLiteralVariable >> isGlobalVariable [
	^ variable isGlobalVariable
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

	^ variable name
]

{ #category : #reading }
OCLiteralVariable >> read [
	^ variable read 
]

{ #category : #accessing }
OCLiteralVariable >> value [
	^variable value
]

{ #category : #accessing }
OCLiteralVariable >> variable [
	^ variable
]

{ #category : #initializing }
OCLiteralVariable >> variable: aLiteralVariable [

	variable := aLiteralVariable
]
