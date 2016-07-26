groups of shades
lightShades: thisMany
	"An array of thisMany colors from white to self. Very useful for displaying color based on a variable in your program.  "
	"(Color red lightShades: 12) inspect"

	^ self class white mix: self shades: thisMany
