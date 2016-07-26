*FreeType-addition
widthAndKernedWidthOfLeft: leftCharacter right: rightCharacterOrNil into: aTwoElementArray
	"Set the first element of aTwoElementArray to the width of leftCharacter and 
	the second element to the width of left character when kerned with
	rightCharacterOrNil. Answer aTwoElementArray"
	| w k |
	w := self widthOf: leftCharacter.
	rightCharacterOrNil 
		ifNil: [
			aTwoElementArray 
				at: 1 put: w; 
				at: 2 put: w]
		ifNotNil: [
			k := self kerningLeft: leftCharacter right: rightCharacterOrNil.
			aTwoElementArray 
				at: 1 put: w; 
				at: 2 put: w+k].
	^ aTwoElementArray
	