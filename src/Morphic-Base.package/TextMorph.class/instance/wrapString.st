menu
wrapString
	"Answer the string to put in a menu that will invite the user to 
	switch autoFit mode"
	^ (wrapFlag
		ifTrue: ['<yes>']
		ifFalse: ['<no>'])
		, 'text wrap to bounds' translated