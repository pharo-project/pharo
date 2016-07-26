adding
grow
	"Become larger in capacity."
	| newArray |
	newArray := Array new: array size * 2.
	newArray replaceFrom: 1 to: array size with: array startingAt: 1.
	array := newArray