"
I model ""super"" keyword
"
Class {
	#name : #SuperVariable,
	#superclass : #ReservedVariable,
	#category : #'OpalCompiler-Core-Semantics'
}

{ #category : #accessing }
SuperVariable class >> semanticNodeClass [

	^RBSuperNode 
]

{ #category : #emitting }
SuperVariable >> emitValue: methodBuilder [
	"super references the receiver, send that follows is a super send (the message lookup starts in the superclass)"
	methodBuilder pushReceiver
]

{ #category : #initialization }
SuperVariable >> initialize [
	super initialize.
	
	name := 'super'
]

{ #category : #testing }
SuperVariable >> isSuperVariable [
	^true
]

{ #category : #debugging }
SuperVariable >> readInContext: aContext [
	^aContext receiver
]

{ #category : #queries }
SuperVariable >> usingMethods [
	"as super is just a push Self, this detects real super sends, not accesses to super which 
	should never happen"
	^ SystemNavigation new allMethods select: [ :method | 
		  method sendsToSuper ]
]
