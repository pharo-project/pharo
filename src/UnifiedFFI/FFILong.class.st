"
I'm a long. 
Longs are special, see FFIULong comment for details.
"
Class {
	#name : #FFILong,
	#superclass : #FFIULong,
	#category : #UnifiedFFI-Types
}

{ #category : #accessing }
FFILong class >> externalType [ 
	^ FFIArchitecture forCurrentArchitecture externalLongType
]

{ #category : #private }
FFILong >> basicHandle: aHandle at: index [
	^ aHandle platformLongAt: index
]

{ #category : #private }
FFILong >> basicHandle: aHandle at: index put: value [ 
	^ aHandle platformLongAt: index put: value
]

{ #category : #'private emitting code' }
FFILong >> emitSelector [
	^ 'platformLongAt'
]

{ #category : #'private emitting code' }
FFILong >> offsetPointerReadFieldAt: offsetVariableName [
	^ '^ExternalData 
		fromHandle: (handle {1}: {2})
		type: FFIArchitecture forCurrentArchitecture externalLongType asPointerType'
	format: { 
		self emitSelector.
		offsetVariableName }
]
