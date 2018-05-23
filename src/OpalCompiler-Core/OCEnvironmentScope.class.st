"
I am the glonal environment. The default is Smalltalk globals
"
Class {
	#name : #OCEnvironmentScope,
	#superclass : #OCAbstractScope,
	#instVars : [
		'environment'
	],
	#category : #'OpalCompiler-Core-Semantics'
}

{ #category : #'instance creation' }
OCEnvironmentScope class >> for: anEnvironment [  
	^self new environment: anEnvironment
]

{ #category : #accessing }
OCEnvironmentScope >> environment: aCollection [ 
	environment := aCollection
]

{ #category : #lookup }
OCEnvironmentScope >> lookupVar: name [
	"Return a var with this name.  Return nil if none found"

	^(environment bindingOf: name asSymbol) ifNotNil: [:assoc | 
		OCLiteralVariable new 
			assoc: assoc; 
			scope: self; 
			yourself]

]

{ #category : #lookup }
OCEnvironmentScope >> lookupVar: name inContext: aContext [
	"I'll lookup a variable in the (global) environment. The context does not play any role in here because variables in classes are statically defined"

	^ self lookupVar: name
]

{ #category : #creation }
OCEnvironmentScope >> newClassScope: aClass [

	^ (OCClassScope for: aClass) outerScope: self; yourself
]
