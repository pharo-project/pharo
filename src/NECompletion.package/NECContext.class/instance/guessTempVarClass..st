private
guessTempVarClass: aSHRange 
	^self guessTempVarClass: (self sourceOf: aSHRange) type: aSHRange type.
