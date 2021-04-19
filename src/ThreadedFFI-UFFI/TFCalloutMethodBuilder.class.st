"
I override some methods to correctly generate TFFI methods when using UFFI.
"
Class {
	#name : #TFCalloutMethodBuilder,
	#superclass : #FFICalloutMethodBuilder,
	#category : #'ThreadedFFI-UFFI'
}

{ #category : #private }
TFCalloutMethodBuilder >> createFFICalloutLiteralFromSpec: functionSpec [
	
	^ 	TFExternalFunction
			name: functionSpec functionName
			moduleName: self libraryName
			parameterTypes: (functionSpec arguments collect: #tfExternalTypeWithArity)
			returnType: functionSpec returnType tfExternalTypeWithArity
			fixedArgumentCount: fixedArgumentCount 
]

{ #category : #private }
TFCalloutMethodBuilder >> generateFFICallout: builder spec: functionSpec ffiLibrary: ffiLibrary [

	TFCalloutAPI isTracing ifTrue: [ 
		TFCalloutAPI trace: sender. 
		builder 
			pushLiteral: TFCalloutAPI;
			pushLiteral: sender;
			send: #trace: ].
	
	builder 
		pushLiteral: (ffiLibrary uniqueInstance runner).
			
	"save ffi call as literal"
	builder pushLiteral: (self createFFICalloutLiteralFromSpec: functionSpec).
	"iterate arguments in order (in the function) to create the function call"
	
	functionSpec arguments 
		do: [ :each | 
			each emitArgument: builder context: sender inCallout: self requestor.
			each resolvedType tfExternalTypeWithArity emitMarshallToPrimitive: builder ].
	
	"create the array"
	builder pushConsArray: functionSpec arguments size.
	builder addTemp: #argumentsArray.
	builder storeTemp: #argumentsArray.
	
	"send call and store into result"
	builder send: #invokeFunction:withArguments:.

	functionSpec arguments withIndexDo: [ :each :index| 
		each emitReturnArgument: builder context: sender.
		each resolvedType tfExternalTypeWithArity 
			emitFreeIfNeededOfIndex: index
			argumentsArrayTempName: #argumentsArray
			withBuilder: builder  ].
	
	"Additional marshall in the case of TFFI"	
	functionSpec returnType resolvedType tfExternalTypeWithArity emitMarshallFromPrimitive: builder.

	"convert in case return type needs it. And return reseult"	
	
	^ functionSpec returnType
		emitReturn: builder
		resultTempVar: #result
		context: sender
		inCallout: self requestor
]
