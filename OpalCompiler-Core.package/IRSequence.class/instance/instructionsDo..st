successor sequences
instructionsDo: aBlock

	^self withAllSuccessorsDo: [:seq | seq do: aBlock].