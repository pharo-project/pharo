Extension { #name : #Encoder }

{ #category : #'*Flashback-Decompiler' }
Encoder class >> skipTemps: numTemps for: aDecompiler [
	numTemps timesRepeat: [ aDecompiler goToNextInstruction ]
]
