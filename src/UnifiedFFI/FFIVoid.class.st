"
I'm a void type. 
I'm used always as pointer type, otherwise it does not have any sense. 
"
Class {
	#name : #FFIVoid,
	#superclass : #FFIExternalType,
	#category : #UnifiedFFI-Types
}

{ #category : #accessing }
FFIVoid class >> externalType [ 
	^ ExternalType void
]

{ #category : #private }
FFIVoid >> basicHandle: aHandle at: index [
	self error: 'You can not access plain void types (it does not has sense)'
]

{ #category : #private }
FFIVoid >> basicHandle: aHandle at: index put: value [
	self error: 'You can not access plain void types (it is meaningless)'
]

{ #category : #accessing }
FFIVoid >> callbackReturnOn: callbackContext for: anObject [
	"Void returns does not set anything"
	self isPointer ifFalse: [ ^ nil ]. 
	^ callbackContext wordResult: anObject asInteger.

]

{ #category : #'emitting code' }
FFIVoid >> emitPointerReturn: aBuilder resultTempVar: resultVar context: aContext [
	^ aBuilder 
		addTemp: #tmpResult;
		"keep invoke result into the tmpResult var"
		storeTemp: #tmpResult;
		popTop;
		"return := self objectClass basicNew"
		pushLiteralVariable: self objectClass binding;
		send: #basicNew;
		storeTemp: resultVar;
		popTop;
		"return instVarAt: (index of argName) put: tmpResult"
		pushTemp: resultVar;
		pushLiteral: (self objectClass 
			instVarIndexFor: #handle 
			ifAbsent: [ self error: 'No handle instVar' ]);
		pushTemp: #tmpResult;
		send: #instVarAt:put:;
		pushTemp: resultVar;
		returnTop	
]

{ #category : #'emitting code' }
FFIVoid >> emitReturn: aBuilder resultTempVar: resultVar context: aContext [ 
	self isPointer 
		ifTrue: [ ^ super emitReturn: aBuilder resultTempVar: resultVar context: aContext ].
	^ self emitSelfReturn: aBuilder
]

{ #category : #'emitting code' }
FFIVoid >> emitSelfReturn: aBuilder [
	^ aBuilder 
		popTop;
		pushReceiver;
		returnTop
]

{ #category : #accessing }
FFIVoid >> externalTypeSize [ 
	self pointerArity < 1 ifTrue: [ ^ self error: 'void has unknown size' ].
	^ self pointerSize 
]

{ #category : #testing }
FFIVoid >> isVoid [
	^ true
]

{ #category : #testing }
FFIVoid >> needsArityPacking [
	"Void just have sense if they are pointers, so the roll function just have sense for bigger
	 arities"
	^ self pointerArity > 1
]

{ #category : #testing }
FFIVoid >> needsArityUnpacking [
	"Void just have sense if they are pointers, so the unpack function just have sense 
	 if they have been rolled before"
	^ self needsArityPacking
]
