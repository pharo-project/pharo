accessing
newSource
	replacements ifNil: [^self formattedCode].
	^[self reformatSource] on: Error do: [:ex | ex return: self formattedCode]