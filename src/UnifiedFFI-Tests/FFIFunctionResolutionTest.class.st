Class {
	#name : #FFIFunctionResolutionTest,
	#superclass : #FFIAbstractTest,
	#instVars : [
		'stack'
	],
	#category : #'UnifiedFFI-Tests-Tests'
}

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

	self assert: argument resolvedType class equals: intType
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
	argument := FFIConstantArgument new
		value: false;
		yourself.
	
	resolver := FFICallout new
		requestor: self;
		yourself.
	
	argument resolveUsing: resolver.

	self assert: argument resolvedType class equals: int32Type
]

{ #category : #tests }
FFIFunctionResolutionTest >> testResolveConstantIntegerShouldSetConstantLoader [

	| argument |
	argument := FFIConstantArgument new
		value: 1;
		yourself.
	
	argument resolveUsing: FFICallout new.
	argument loader emitArgument: self context: nil.

	self assert: stack pop equals: { #literal . 1}.
]

{ #category : #tests }
FFIFunctionResolutionTest >> testResolveConstantNULLShouldSetConstantNullLoader [

	| argument |
	argument := FFIConstantArgument new
		value: 'NULL';
		yourself.
	
	argument resolveUsing: FFICallout new.
	argument loader emitArgument: self context: nil.

	self assert: stack pop equals: { #literal . ExternalAddress null}.
]

{ #category : #tests }
FFIFunctionResolutionTest >> testResolveConstantNilShouldSetConstantNullLoader [

	| argument |
	argument := FFIConstantArgument new
		value: nil;
		yourself.
	
	argument resolveUsing: FFICallout new.
	argument loader emitArgument: self context: nil.

	self assert: stack pop equals: { #literal . ExternalAddress null }.
]

{ #category : #tests }
FFIFunctionResolutionTest >> testResolveConstantSelfOfExternalObjectShouldSetConstantFirstInstanceVariableLoader [

	| argument receiver context resolver |
	argument := FFIConstantArgument new
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

	self assert: stack pop equals: #(#ivar 1).
]

{ #category : #tests }
FFIFunctionResolutionTest >> testResolveConstantSelfShouldResolveToExternalObjectType [

	| argument receiver context resolver |
	argument := FFIConstantArgument new
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

	self assert: argument resolvedType equals: (receiver class asExternalTypeOn: nil)
]

{ #category : #tests }
FFIFunctionResolutionTest >> testResolveConstantTrueShouldSetConstantOneLoader [

	| argument |
	argument := FFIConstantArgument new
		value: true;
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
