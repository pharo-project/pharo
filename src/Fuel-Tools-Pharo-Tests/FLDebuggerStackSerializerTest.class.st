Class {
	#name : #FLDebuggerStackSerializerTest,
	#superclass : #TestCase,
	#category : #'Fuel-Tools-Pharo-Tests'
}

{ #category : #tests }
FLDebuggerStackSerializerTest >> testBuildFileNameForContext [

	| stackSerializer context |
	stackSerializer := FLDebuggerStackSerializer new.

	context := [  ] asContext.
	stackSerializer buildFileNameForContext: context.

	self deny: stackSerializer fileName equals: nil.
	self assert:
		('Debugger-Stack-' , context receiver class instanceSide name
		 , '-*.fuel' match: stackSerializer fileName)
]

{ #category : #tests }
FLDebuggerStackSerializerTest >> testSerializeContextToFileNamed [

	| context file |
	context := [  ] asContext.
	file := 'serialized-stack' asFileReference.
	FLDebuggerStackSerializer
		serializeContext: context
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
