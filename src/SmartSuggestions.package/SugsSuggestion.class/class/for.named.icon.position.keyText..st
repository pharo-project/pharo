instance creation
for: blockCommand named: label icon: icon position: position keyText: key
	^ (self for: blockCommand named: label icon: icon position: position)
		  keyText: key;
		  yourself.