"
I represent the addition of a method to a class.  I can produce the CompiledMethod, install it, and then notify the system that the method has been added.  This allows Monticello to implement atomic addition.  A loader can compile all classes and methods first and then install all methods only after they have been all compiled, and in a way that executes little code.
"
Class {
	#name : 'MethodAddition',
	#superclass : 'Object',
	#instVars : [
		'text',
		'changeStamp',
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
MethodAddition >> compile: aString classified: aProtocolName withStamp: aString2 inClass: aClass [

	text := aString.
	protocolName := aProtocolName.
	changeStamp := aString2.
	myClass := aClass.
	compiler := myClass compiler
]

{ #category : 'operations' }
MethodAddition >> installMethod [

	| priorMethod |
	compiledMethod := compiler
		                  permitUndeclared: true;
		                  compile: text asString.

	priorMethod := myClass compiledMethodAt: compiledMethod selector ifAbsent: [  ].

	compiledMethod
		putSource: text asString
		class: myClass
		protocol: (myClass ensureProtocol: protocolName)
		withStamp: changeStamp
		priorMethod: priorMethod.

	myClass addAndClassifySelector: compiledMethod selector withMethod: compiledMethod inProtocol: protocolName.

	"The following code doesn't seem to do anything."
	myClass instanceSide noteCompilationOf: compiledMethod meta: myClass isClassSide.
	^ compiledMethod
]
