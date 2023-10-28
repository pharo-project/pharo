"
I represent the addition of a method to a class.  I can produce the CompiledMethod, install it, and then notify the system that the method has been added.  This allows Monticello to implement atomic addition.  A loader can compile all classes and methods first and then install all methods only after they have been all compiled, and in a way that executes little code.
"
Class {
	#name : 'MethodAddition',
	#superclass : 'Object',
	#instVars : [
		'text',
		'changeStamp',
		'logSource',
		'myClass',
		'selector',
		'compiledMethod',
		'protocolName',
		'priorProtocol'
	],
	#category : 'Monticello-Loading',
	#package : 'Monticello',
	#tag : 'Loading'
}

{ #category : 'compilation' }
MethodAddition >> compile [
	"This method is the how compiling a method used to work.  All these steps were done at once.
     This method should not normally be used, because the whole point of MethodAddition is to let
	you first create a compiled method and then install the method later."

	self installMethod.
	^ selector
]

{ #category : 'compilation' }
MethodAddition >> compile: aString classified: aProtocolName withStamp: aString2 logSource: aBoolean inClass: aClass [

	text := aString.
	protocolName := aProtocolName.
	changeStamp := aString2.
	logSource := aBoolean.
	myClass := aClass
]

{ #category : 'operations' }
MethodAddition >> installMethod [

	('Compiling ' , text asString , ' into : ' , myClass name) traceCr.
	compiledMethod := myClass compiler
		                  source: text asString;
		                  permitUndeclared: true;
		                  protocol: protocolName;
		                  changeStamp: changeStamp;
		                  logged: logSource;
		                  install.
	selector := compiledMethod selector
]

{ #category : 'accessing' }
MethodAddition >> priorProtocol [
	^ priorProtocol
]

{ #category : 'operations' }
MethodAddition >> writeSourceToLog [

	logSource ifTrue: [
		myClass
			logMethodSource: text
			forMethod: compiledMethod
			inProtocol: (myClass ensureProtocol: protocolName)
			withStamp: changeStamp ]
]
