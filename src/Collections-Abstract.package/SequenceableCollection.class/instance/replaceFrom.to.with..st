accessing
replaceFrom: start to: stop with: replacement 
	"This destructively replaces elements from start to stop in the receiver. 
	Answer the receiver itself. Use copyReplaceFrom:to:with: for 
	insertion/deletion which may alter the size of the result."

	replacement size = (stop - start + 1)
		ifFalse: [self error: 'Size of replacement doesnt match'].
	^self replaceFrom: start to: stop with: replacement startingAt: 1