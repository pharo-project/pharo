reading
nextEntry

	^ OmCurrentGlobalName
		value: store globalName
		during: [
			self stonReader
				reset;
				next ]