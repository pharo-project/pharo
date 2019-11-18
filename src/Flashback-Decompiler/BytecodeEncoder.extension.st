Extension { #name : #BytecodeEncoder }

{ #category : #'*Flashback-Decompiler' }
BytecodeEncoder class >> skipTemps: numTemps for: aDecompiler [
	numTemps timesRepeat: [ aDecompiler goToNextInstruction ]
]
