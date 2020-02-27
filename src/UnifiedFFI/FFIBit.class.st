"
I'm a signed bit type. 
See my superclass for an explanation on why this exits.
"
Class {
	#name : #FFIBit,
	#superclass : #FFIUBit,
	#category : #'UnifiedFFI-Types'
}

{ #category : #accessing }
FFIBit class >> externalType [ 
	^ FFIArchitecture forCurrentArchitecture externalLongType
]

{ #category : #private }
FFIBit >> basicHandle: aHandle at: index [
	^ aHandle platformBitAt: index
]

{ #category : #private }
FFIBit >> basicHandle: aHandle at: index put: value [ 
	^ aHandle platformBitAt: index put: value
]

{ #category : #'private emitting code' }
FFIBit >> emitSelector [
	^ 'platformBitAt'
]

{ #category : #'private emitting code' }
FFIBit >> offsetPointerReadFieldAt: offsetVariableName [
	^ '^ExternalData 
		fromHandle: (handle {1}: {2})
		type: FFIArchitecture forCurrentArchitecture externalBitType asPointerType'
	format: { 
		self emitSelector.
		offsetVariableName }
]
