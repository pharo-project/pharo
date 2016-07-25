instance creation
for: blockCommand named: label icon: icon position: position keyText: key  isValidBlock: validBlock
	^ (self for: blockCommand named: label icon: icon position: position)
		  keyText: key;
		 isValidBlock: validBlock;
		  yourself.