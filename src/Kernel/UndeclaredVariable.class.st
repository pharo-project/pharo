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
UndeclaredVariable class >> possiblyRegisteredWithName: aString [

	"Get a registered undeclared variable if any, or a new unregistered undeclared variable"

	| varName |
	varName := aString asSymbol.

	Undeclared associationAt: varName ifPresent: [ :found |
		found class == Association ifTrue: [
			"Found undeclared var can be an association during current bootstrap process.
			So here we should convert it to real variable object otherwise the build will be broken"
			found becomeForward: (self named: varName)].
		^found	].

	^self named: varName
]

{ #category : #'instance creation' }
UndeclaredVariable class >> registeredWithName: aString [

	| var |
	var := self possiblyRegisteredWithName: aString.
	(var isUndeclaredVariable and: [ var isRegistered not ]) ifTrue: [ var register ].
	^ var
]

{ #category : #queries }
UndeclaredVariable >> definingClass [
	"Nobody defines undeclared variable"
	^nil
]

{ #category : #'code generation' }
UndeclaredVariable >> emitStore: methodBuilder [

	super emitStore: methodBuilder
]

{ #category : #'code generation' }
UndeclaredVariable >> emitValue: methodBuilder [
	
super emitValue: methodBuilder
]

{ #category : #registry }
UndeclaredVariable >> isRegistered [
	^Undeclared includesKey: name
]

{ #category : #testing }
UndeclaredVariable >> isUndeclaredVariable [

	^ true
]

{ #category : #'meta-object-protocol' }
UndeclaredVariable >> read [

	<debuggerCompleteToSender>
	self error: 'Cannot read undeclared variable ' , self name
]

{ #category : #registry }
UndeclaredVariable >> register [
	Undeclared add: self
]

{ #category : #registry }
UndeclaredVariable >> unregister [
	Undeclared removeKey: name ifAbsent: [ ]
]

{ #category : #'meta-object-protocol' }
UndeclaredVariable >> write: aValue [

	<debuggerCompleteToSender>
	self error: 'Cannot write undeclared variable ' , self name
]
