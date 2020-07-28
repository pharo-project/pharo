"
I model ""self"" keyword 
"
Class {
	#name : #SelfVariable,
	#superclass : #ReservedVariable,
	#category : #'OpalCompiler-Core-Semantics'
}

{ #category : #accessing }
SelfVariable class >> semanticNodeClass [

	^RBSelfNode 
]

{ #category : #emitting }
SelfVariable >> emitValue: methodBuilder [

	methodBuilder pushReceiver
]

{ #category : #initialization }
SelfVariable >> initialize [
	super initialize.
	
	name := 'self'
]

{ #category : #testing }
SelfVariable >> isSelfVariable [
	^true
]

{ #category : #debugging }
SelfVariable >> readInContext: aContext [
	^aContext receiver
]

{ #category : #queries }
SelfVariable >> usingMethods [
	"as super sends are doing a pushSelf, too, we need to still check the AST level sometimes"

	^ SystemNavigation new allMethods select: [ :method | 
		  method readsSelf and: [ 
			  method sendsToSuper not or: [ 
				  method ast variableNodes anySatisfy: [ :varNode | 
					  varNode variable == self ] ] ] ]
]
