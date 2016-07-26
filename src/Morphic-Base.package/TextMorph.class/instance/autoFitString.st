menu
autoFitString
	"Answer the string to put in a menu that will invite the user to 
	switch autoFit mode"
	^ (self isAutoFit
		ifTrue: ['<yes>']
		ifFalse: ['<no>'])
		, 'text auto fit' translated