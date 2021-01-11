Extension { #name : #FFIConst }

{ #category : #'*ThreadedFFI-UFFI' }
FFIConst >> emitArgument: aBuilder context: aContext [ 

	aBuilder pushLiteral: self value
]

{ #category : #'*ThreadedFFI-UFFI' }
FFIConst >> tfExternalType [

	^ self type tfExternalType
]
