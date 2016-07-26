private
fontAt: index 
	"This is private because no object outside TextStyle should depend on the 
	representation of the font family in fontArray."

	^ fontArray atPin: index