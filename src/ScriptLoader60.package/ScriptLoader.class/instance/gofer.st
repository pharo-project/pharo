accessing
gofer
	^ Smalltalk globals
		at: #Gofer
		ifAbsent: [ 
			self
				installGofer;
				gofer ]