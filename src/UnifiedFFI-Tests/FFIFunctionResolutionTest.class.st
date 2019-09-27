Class {
	#name : #FFIFunctionResolutionTest,
	#superclass : #TestCase,
	#instVars : [
		'stack',
		'intType',
		'int32Type'
	],
	#category : #'UnifiedFFI-Tests-Tests'
}

{ #category : #helpers }
FFIFunctionResolutionTest >> externalTypeAlias: aTypeName [

	"Prefix the type so we have control over it.
	If it is already prefixed, do not prefix it (otherwise this loops)."
	(aTypeName beginsWith: '_test_type_')
		ifTrue: [ ^ aTypeName ].
	^ '_test_type_', aTypeName
]

{ #category : #helpers }
FFIFunctionResolutionTest >> ffiBindingOf: aString [
	
	aString = '_test_type_int'
		ifTrue: [ ^ aString -> intType ].
	aString = '_test_type_bool'
		ifTrue: [ ^ aString -> int32Type ].
	self error: 'Type not recognized: ', aString
]

{ #category : #helpers }
FFIFunctionResolutionTest >> pushInstVar: anInteger [ 
	
	stack push: {#ivar . anInteger}
]

{ #category : #helpers }
FFIFunctionResolutionTest >> pushLiteral: aLiteral [

	stack push: { #literal . aLiteral }
]

{ #category : #helpers }
FFIFunctionResolutionTest >> pushLiteralVariable: aClassVariable [ 
	
	stack push: { #classVariable . aClassVariable }
]

{ #category : #helpers }
FFIFunctionResolutionTest >> pushReceiver [
	
	stack push: #( self )
]

{ #category : #helpers }
FFIFunctionResolutionTest >> pushTemp: aString [ 
	
	stack push: { #temp . aString }
]

{ #category : #running }
FFIFunctionResolutionTest >> setUp [

	super setUp.
	stack := Stack new.
	intType := FFITestType new.
	int32Type := FFITestType new.
]

{ #category : #tests }
FFIFunctionResolutionTest >> testResolveArgumentVariableShouldSetArgumentVariableLoader [

	| argument receiver context resolver |
	argument := FFIVariableArgument
		name: 'arg'
		typeName: 'int'
		arity: 0.

	receiver := 17@25.
	context := Context
		sender: nil
		receiver: receiver
		method: Point>>#*
		arguments: #().
	resolver := FFICallout new
		sender: context;
		yourself.
	
	argument resolveUsing: resolver.
	argument loader emitArgument: self context: context.

	self assert: stack pop equals: { #temp . 'arg' }
]

{ #category : #tests }
FFIFunctionResolutionTest >> testResolveArgumentVariableWithExplicitTypeResolvesType [

	| argument receiver context resolver |
	argument := FFIVariableArgument
		name: 'arg'
		typeName: 'int'
		arity: 0.

	receiver := 17@25.
	context := Context
		sender: nil
		receiver: receiver
		method: Point>>#*
		arguments: #().
		
	resolver := FFICallout new
		sender: context;
		requestor: self;
		yourself.
	
	argument resolveUsing: resolver.

	self assert: argument resolvedType equals: intType
]

{ #category : #tests }
FFIFunctionResolutionTest >> testResolveClassVariableShouldSetClassVariableLoader [

	| argument receiver context resolver |
	argument := FFIVariableArgument
		name: 'AsciiOrder'
		typeName: 'int'
		arity: 0.

	receiver := 'aString'.
	context := Context
		sender: nil
		receiver: receiver
		method: String>>#asDate
		arguments: #().
	resolver := FFICallout new
		sender: context;
		yourself.
	
	argument resolveUsing: resolver.
	argument loader emitArgument: self context: context.

	self assert: stack pop equals: { #classVariable . (String classPool bindingOf: #AsciiOrder )}
]

{ #category : #tests }
FFIFunctionResolutionTest >> testResolveConstantFalseShouldSetConstantZeroLoader [

	| argument resolver |
	argument := FFIValueArgument new
		value: 'false';
		yourself.
	
	resolver := FFICallout new
		requestor: self;
		yourself.
	
	argument resolveUsing: resolver.

	self assert: argument resolvedType equals: int32Type
]

{ #category : #tests }
FFIFunctionResolutionTest >> testResolveConstantIntegerShouldSetConstantLoader [

	| argument |
	argument := FFIValueArgument new
		value: 1;
		yourself.
	
	argument resolveUsing: FFICallout new.
	argument loader emitArgument: self context: nil.

	self assert: stack pop equals: { #literal . 1}.
]

{ #category : #tests }
FFIFunctionResolutionTest >> testResolveConstantNULLShouldSetConstantNullLoader [

	| argument |
	argument := FFIValueArgument new
		value: 'NULL';
		yourself.
	
	argument resolveUsing: FFICallout new.
	argument loader emitArgument: self context: nil.

	self assert: stack pop equals: { #literal . ExternalAddress null}.
]

{ #category : #tests }
FFIFunctionResolutionTest >> testResolveConstantNilShouldSetConstantNullLoader [

	| argument |
	argument := FFIValueArgument new
		value: 'nil';
		yourself.
	
	argument resolveUsing: FFICallout new.
	argument loader emitArgument: self context: nil.

	self assert: stack pop equals: { #literal . ExternalAddress null }.
]

{ #category : #tests }
FFIFunctionResolutionTest >> testResolveConstantSelfShouldSetConstantSelfLoader [

	| argument receiver context resolver |
	argument := FFIValueArgument new
		value: 'self';
		yourself.
		
	receiver := FFIExternalObjectForTest new.
	context := Context
		sender: nil
		receiver: receiver
		method: FFIExternalObjectForTest>>#testMethod
		arguments: #().
		
	resolver := FFICallout new
		sender: context;
		yourself.
	
	argument resolveUsing: resolver.
	argument loader emitArgument: self context: context.

	self assert: stack pop equals: #(#self).
]

{ #category : #tests }
FFIFunctionResolutionTest >> testResolveConstantTrueShouldSetConstantOneLoader [

	| argument |
	argument := FFIValueArgument new
		value: 'true';
		yourself.
	
	argument resolveUsing: FFICallout new.
	argument loader emitArgument: self context: nil.

	self assert: stack pop equals: { #literal . 1 }.
]

{ #category : #tests }
FFIFunctionResolutionTest >> testResolveInstanceVariableShouldSetInstanceVariableLoader [

	| argument receiver context resolver |
	argument := FFIVariableArgument
		name: 'x'
		typeName: 'int'
		arity: 0.

	receiver := 17@25.
	context := Context
		sender: nil
		receiver: receiver
		method: Point>>#abs
		arguments: #().
	resolver := FFICallout new
		sender: context;
		yourself.
	
	argument resolveUsing: resolver.
	argument loader emitArgument: self context: context.

	self assert: stack pop equals: #(ivar 1)
]
