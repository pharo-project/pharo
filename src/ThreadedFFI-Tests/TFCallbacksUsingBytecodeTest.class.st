Class {
	#name : 'TFCallbacksUsingBytecodeTest',
	#superclass : 'TFCallbacksTest',
	#category : 'ThreadedFFI-Tests',
	#package : 'ThreadedFFI-Tests'
}

{ #category : 'building suites' }
TFCallbacksUsingBytecodeTest class >> testParameters [

	^ ParametrizedTestMatrix new
		forSelector: #runner addOptions: { [ TFSameThreadRunner uniqueInstance ] }
]

{ #category : 'executing' }
TFCallbacksUsingBytecodeTest >> invokeFunction: fun withArguments: arguments [

	| argumentNames builder method |
	argumentNames := arguments collectWithIndex: [ :e :i | #_arg_ , i printString ].

	fun validate.

	builder := OCIRBuilder new
		numArgs: arguments size;
		addTemps: argumentNames.

	argumentNames do: [ :aName | builder pushTemp: aName ].
	builder sameThreadCallout: fun.
	
	fun definition returnType isVoid 
		ifTrue: [ builder pushReceiver; returnTop ]
		ifFalse: [ builder returnTop ].
		
	method := builder ir compiledMethod.
	^ method valueWithReceiver: nil arguments: arguments.
]
