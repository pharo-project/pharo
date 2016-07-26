instance creation
for: blockCommand named: label icon: icon position: position isValidBlock: validBlock
	^ (self for: blockCommand named: label icon: icon position: position)
   		  isValidBlock: validBlock;
		  yourself.