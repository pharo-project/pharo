Extension { #name : #EncoderForSistaV1 }

{ #category : #'*Flashback-Decompiler' }
EncoderForSistaV1 class >> skipTemps: numTemps for: aDecompiler [
	numTemps > 0 ifTrue: [ aDecompiler goToNextInstruction ]
]
