accessing
preferGradientFill 
	"Answer the value of preferGradientFill "

	^preferGradientFill  ifNil: [preferGradientFill := self preferGradientFillByDefault]