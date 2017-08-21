initialize
createTempVectorNamed: name withVars: anArray
	" Don't add the temp yet, we only know it's index at the end of the block or method "
	"self addVectorTemps: anArray"
	" Update the byte index to point before the pushing of the new vector "
	sourceMapByteIndex := sourceMapByteIndex - 2.
	self add: (IRInstruction createTempVectorNamed: name withVars: anArray)