"
Float type. 
Represented as double in C side.
"
Class {
	#name : #FFIFloat64,
	#superclass : #FFIFloatType,
	#category : #'UnifiedFFI-Types'
}

{ #category : #accessing }
FFIFloat64 class >> externalType [ 
	^ ExternalType double
]

{ #category : #accessing }
FFIFloat64 class >> externalTypeAlignment [
	^ OSPlatform current ffiFloat64Alignment
]

{ #category : #accessing }
FFIFloat64 class >> externalTypeSize [
	^ 8
]

{ #category : #private }
FFIFloat64 >> basicHandle: aHandle at: index [
	^ aHandle doubleAt: index
]

{ #category : #private }
FFIFloat64 >> basicHandle: aHandle at: index put: value [
	^ aHandle doubleAt: index put: value
]

{ #category : #'emitting code' }
FFIFloat64 >> emitPointerArityRoll: aBuilder context: aContext [
	self loader 
		emitPointerArityPack: aBuilder 
		context: aContext 
		arity: self pointerArity
		selector: #packAsDoubleToArity:
]
