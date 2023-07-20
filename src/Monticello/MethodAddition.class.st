"
I represent the addition of a method to a class.  I can produce the CompiledMethod, install it, and then notify the system that the method has been added.  This allows Monticello to implement atomic addition.  A loader can compile all classes and methods first and then install all methods only after they have been all compiled, and in a way that executes little code.
"
Class {
	#name : #MethodAddition,
	#superclass : #Object,
	#instVars : [
		'text',
		'changeStamp',
		'logSource',
		'myClass',
		'protocolName'
	],
	#category : #'Monticello-Loading'
}

{ #category : #compilation }
MethodAddition >> compile [

	self deprecated: 'Use #installMethod instead.' transformWith: '`@rcv compile' -> '`@rcv installMethod'.
	^ self installMethod
]

{ #category : #compilation }
MethodAddition >> compile: aString classified: aProtocolName withStamp: aString2 logSource: aBoolean inClass: aClass [

	text := aString.
	protocolName := aProtocolName.
	changeStamp := aString2.
	logSource := aBoolean.
	myClass := aClass
]

{ #category : #operations }
MethodAddition >> installMethod [

	^ (myClass compiler
		   permitUndeclared: true;
		   protocol: protocolName;
		   changeStamp: changeStamp;
		   logSource: logSource;
		   compile: text asString) selector
]
