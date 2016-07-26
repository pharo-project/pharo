groups of shades
darkShades: thisMany
	"An array of thisMany colors from black to the receiver.  Array is of length num. Very useful for displaying color based on a variable in your program.  "
	"(Color red darkShades: 12) inspect"

	^ self class black mix: self shades: thisMany
