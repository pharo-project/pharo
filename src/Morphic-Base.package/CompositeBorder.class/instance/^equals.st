comparing
= aBorderStyle
	"Check the sub-borders too"
	
	^super = aBorderStyle and: [
		self borders = aBorderStyle borders]