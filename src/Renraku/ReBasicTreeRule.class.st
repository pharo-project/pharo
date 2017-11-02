"
I'm a basic root for all ast-based rules. My basicCheck: gets a method AST as the parameter
"
Class {
	#name : #ReBasicTreeRule,
	#superclass : #ReAbstractRule,
	#category : 'Renraku-Deprecated'
}

{ #category : #testing }
ReBasicTreeRule class >> checksNode [

	^ self name ~= #ReBasicTreeRule
]

{ #category : #running }
ReBasicTreeRule >> basicCheck: anAST [
	self 
		deprecated: 'Please subclass from ReNodeBasedRule and treat the passed entity as in can be any AST node'
		on: 	'11 July 2016' 
		in: 'Pharo-6.0-60143-'.
	^ super basicCheck: anAST 
]

{ #category : #running }
ReBasicTreeRule >> checkMethod: aMethod [

	^ (self basicCheck: aMethod ast)
]
