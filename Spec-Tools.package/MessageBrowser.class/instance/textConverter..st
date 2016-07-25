accessing
textConverter: aTextConverter

	textConverterHolder value: (aTextConverter method: self textConverter method).
	textModel aboutToStyle: self textConverter shouldShout .
	textModel text: self textConverter getText.