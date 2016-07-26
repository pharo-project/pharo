private
buildArrayString: aCollection
	
	^ String streamContents: [:stream | 
		stream << '#('.
		aCollection do: [:each |  
			stream  nextPutAll: '#('.
			stream nextPutAll: each first printString.
			stream  nextPutAll: ' '.
			stream nextPutAll: (each at: 2) asSymbol  printString.
			stream  nextPutAll: ') '.].
		stream << ')']