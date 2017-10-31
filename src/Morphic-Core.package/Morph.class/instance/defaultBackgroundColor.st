initialize
defaultBackgroundColor
	"Answer the color to be used as the base window color for a window whose model is an object of the receiver's class"
	
	"I don't want to do a self theme  because otherwise I will need to implement it on Object"
	^  Smalltalk ui theme windowColorFor: self