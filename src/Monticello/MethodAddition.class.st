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
		'selector',
		'compiledMethod',
		'protocolName'
	],
	#category : #'Monticello-Loading'
}

{ #category : #compilation }
MethodAddition >> compile [
	"This method is the how compiling a method used to work.  All these steps were done at once.
     This method should not normally be used, because the whole point of MethodAddition is to let
	you first create a compiled method and then install the method later."

	self
		createCompiledMethod;
		installMethod.
	^ selector
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
MethodAddition >> createCompiledMethod [
	"CyrilFerlicot: Why do we need to explicitly write to the source? Why do we call the compiler?
	
	Can't we just call #compile:classfied: here?"

	compiledMethod := myClass compiler
		                  permitUndeclared: true;
		                  compile: text asString.
	selector := compiledMethod selector.
	self writeSourceToLog
]

{ #category : #operations }
MethodAddition >> installMethod [

	myClass addAndClassifySelector: selector withMethod: compiledMethod inProtocol: protocolName
]

{ #category : #operations }
MethodAddition >> writeSourceToLog [

	logSource ifTrue: [
		myClass
			logMethodSource: text
			forMethod: compiledMethod
			inProtocol: (myClass ensureProtocol: protocolName)
			withStamp: changeStamp ]
]
