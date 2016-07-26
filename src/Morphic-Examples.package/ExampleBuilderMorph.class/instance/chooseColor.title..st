services
chooseColor: aColor title: label
	"Answer the user choice of a colour."
	
	^ self theme 
		chooseColorIn: self modalMorph
		title: (label ifNil: ['Choose Color' translated])
		color: aColor