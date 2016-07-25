printing
printString
	"Explicit override of superclass implementation. When you are printing a loadDirective it is annoying to have it truncated."

	^String streamContents: [:s | self printOn: s]