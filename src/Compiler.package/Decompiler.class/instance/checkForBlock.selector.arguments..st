control
checkForBlock: receiver selector: selector arguments: arguments
	[selector == #closureCopy:copiedValues:] assert.
	^self checkForClosureCopy: receiver arguments: arguments