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
		'compiledMethod',
		'protocolName',
		'compiler'
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
	^ compiledMethod selector
]

{ #category : 'compilation' }
MethodAddition >> compile: aString classified: aProtocolName withStamp: aString2 logSource: aBoolean inClass: aClass [

	text := aString.
	protocolName := aProtocolName.
	changeStamp := aString2.
	logSource := aBoolean.
	myClass := aClass.
	compiler := myClass compiler
]

{ #category : 'operations' }
MethodAddition >> installMethod [

	| selector priorMethod priorOrigin priorProtocol |
	compiledMethod := compiler
		                  permitUndeclared: true;
		                  compile: text asString.
	selector := compiledMethod selector.
	self writeSourceToLog.
	priorMethod := myClass compiledMethodAt: selector ifAbsent: [ nil ].
	priorOrigin := compiledMethod origin.
	priorProtocol := myClass protocolOfSelector: selector.
	myClass addAndClassifySelector: selector withMethod: compiledMethod inProtocol: protocolName.

	"The following code doesn't seem to do anything."
	myClass instanceSide noteCompilationOf: compiledMethod meta: myClass isClassSide
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
