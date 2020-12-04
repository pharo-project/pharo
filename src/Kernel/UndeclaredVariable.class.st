"
I model undeclared variable bindings. I am stored as a literal inside methods which reference me.

The compiler forwards bytecode generation to me for accessing the variable. 

In future I can profice logging and user warning when evaluated code accesses undeclared variables
"
Class {
	#name : #UndeclaredVariable,
	#superclass : #LiteralVariable,
	#category : #'Kernel-Variables'
}

{ #category : #'instance creation' }
UndeclaredVariable class >> registeredWithName: aString [
	| varName |
	varName := aString asSymbol.
	
	Undeclared associationAt: varName ifPresent: [ :found | 
		found class == Association ifTrue: [ 
			"Found undeclared var can be an association during current bootstrap process.
			So here we should convert it to real variable object otherwise the build will be broken"
			found becomeForward: (self named: varName)].
		^found	].
	
	^(self named: varName)
		register;
		yourself
]

{ #category : #queries }
UndeclaredVariable >> definingClass [
	"Nobody defines undeclared variable"
	^nil
]

{ #category : #'code generation' }
UndeclaredVariable >> emitStore: methodBuilder [

	methodBuilder storeIntoLiteralVariable: self
]

{ #category : #'code generation' }
UndeclaredVariable >> emitValue: methodBuilder [
	methodBuilder pushLiteralVariable: self
]

{ #category : #registry }
UndeclaredVariable >> isRegistered [ 
	^Undeclared includesKey: name
]

{ #category : #testing }
UndeclaredVariable >> isUndeclaredVariable [

	^ true
]

{ #category : #registry }
UndeclaredVariable >> register [ 
	Undeclared add: self
]

{ #category : #registry }
UndeclaredVariable >> unregister [ 
	Undeclared removeKey: name ifAbsent: [ ]
]
