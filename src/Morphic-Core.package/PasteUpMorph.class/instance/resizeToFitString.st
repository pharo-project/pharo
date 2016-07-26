options
resizeToFitString
	"Answer a string, to be used in a self-updating menu, to 
	represent whether the receiver is currently using resize-to-fit 
	or not"
	^ (self resizeToFit
		ifTrue: ['<yes>']
		ifFalse: ['<no>'])
		, 'resize to fit' translated