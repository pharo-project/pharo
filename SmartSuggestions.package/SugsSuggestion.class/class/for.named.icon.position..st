instance creation
for: blockCommand named: label icon: icon position: position
	^ (self for: blockCommand named: label icon: icon)
	 	  position: position;
		  yourself.