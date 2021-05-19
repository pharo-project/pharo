Class {
	#name : #FLDebuggerStackSerializerTest,
	#superclass : #TestCase,
	#category : #'NewTools-Debugger-Fuel-Tests'
}

{ #category : #tests }
FLDebuggerStackSerializerTest >> testFileNameForContext [
	| stackSerializer context fileName |
	stackSerializer := FLDebuggerStackSerializer new.

	context := [  ] asContext.
	fileName := stackSerializer fileNameForContext: context.

	self deny: fileName equals: nil.
	self assert:
		('Debugger-Stack-' , context receiver class instanceSide name
		 , '-*.fuel' match: fileName)
]

{ #category : #tests }
FLDebuggerStackSerializerTest >> testSerializeContextToFileNamed [
	| context file |
	context := [  ] asContext.
	file := 'serialized-stack' asFileReference.
	FLDebuggerStackSerializer
		serializeStackFromContext: context
		toFileNamed: 'serialized-stack'.

	self assert: file exists.
	self
		should: [ 
		FLMaterializer materializeFromFileNamed: 'serialized-stack' ]
		raise: FLDebuggerStackMaterialization
]

{ #category : #tests }
FLDebuggerStackSerializerTest >> testSerializeStackFromContext [
	| stackSerializer context |
	stackSerializer := FLDebuggerStackSerializer new.
	
	context := [  ] asContext.
	stackSerializer	serializeStackFromContext: context.

	self assert: stackSerializer fileName asFileReference exists.
	self
		should: [ 
		FLMaterializer materializeFromFileNamed: stackSerializer fileName ]
		raise: FLDebuggerStackMaterialization
]
