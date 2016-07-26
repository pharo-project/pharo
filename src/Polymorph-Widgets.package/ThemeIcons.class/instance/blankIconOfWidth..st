utils
blankIconOfWidth: aNumber 
	^ self icons
		at: ('blank-' , aNumber asString) asSymbol
		ifAbsentPut: [Form extent: aNumber @ 1 depth: 8]