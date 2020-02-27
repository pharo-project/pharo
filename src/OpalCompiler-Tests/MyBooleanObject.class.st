"
I am an example class used in MustBeBooleanTest.

The compiler compiles some solectors specially (among them and:, or:, not, ifTrue: and related).

These methods are compiled directly to a ""jump if true/false"" byteode. This means that a method like #ifTrue:
is never called.

When the jump bytecode is called on a non-boolean, the VM sends #mustBeBooleanIn:

- In ST80, this just raises the NonBooleanReceiver exception.
- In Pharo, instead the expression is compiled on the fly without optimization and then executed 
  (read the comment in #mustBeBooleanIn: for more information about the details).

This means that in Pharo, we can define our own kind of Boolean and implement e.g. #ifTrue: there and it will just work as 
expected.

This class contains an example for this and MustBeBooleanTest tests that endeed we can call #ifTrue: on a non-Boolean.
"
Class {
	#name : #MyBooleanObject,
	#superclass : #Object,
	#category : #'OpalCompiler-Tests-Misc'
}

{ #category : #controlling }
MyBooleanObject >> and: alternativeBlock [ 
	"Nonevaluating conjunction -- answer with false since the receiver is false."

	^self
]

{ #category : #controlling }
MyBooleanObject >> ifTrue: aBlock [ 
	^ aBlock value printString , ' sent from my boolean object'
]

{ #category : #'logical operations' }
MyBooleanObject >> not [
	^true
]

{ #category : #controlling }
MyBooleanObject >> or: alternativeBlock [ 
	^alternativeBlock value
]
