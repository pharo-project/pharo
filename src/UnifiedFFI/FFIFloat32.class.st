"
Float type. 
Represented as float in C side.
"
Class {
	#name : #FFIFloat32,
	#superclass : #FFIFloatType,
	#category : #'UnifiedFFI-Types'
}

{ #category : #accessing }
FFIFloat32 class >> externalType [ 
	^ ExternalType float
]

{ #category : #accessing }
FFIFloat32 class >> externalTypeAlignment [
	^ 4
]

{ #category : #accessing }
FFIFloat32 class >> externalTypeSize [
	^ 4
]

{ #category : #private }
FFIFloat32 >> basicHandle: aHandle at: index [
	^ aHandle floatAt: index
]

{ #category : #private }
FFIFloat32 >> basicHandle: aHandle at: index put: value [
	^ aHandle floatAt: index put: value
]

{ #category : #accessing }
FFIFloat32 >> callbackReturnOn: callbackContext for: aFloat [
	^ FFIArchitecture forCurrentArchitecture returnSingleFloatsAsDoubles
		ifTrue: [ callbackContext floatResult: aFloat ]
		ifFalse: [ callbackContext singleFloatResult: aFloat ]
]

{ #category : #'emitting code' }
FFIFloat32 >> emitPointerArityRoll: aBuilder context: aContext [
	self loader 
		emitPointerArityPack: aBuilder 
		context: aContext 
		arity: self pointerArity
		selector: #packAsFloatToArity:
]
