accessing-selection
selection
	"Answer the text in the paragraph that is currently selected."
	^self text copyFrom: self startIndex to: self stopIndex - 1 