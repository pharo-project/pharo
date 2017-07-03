accessing method dictionary
selectorsWithArgs: numberOfArgs
	"Return all selectors defined in this class that take this number of arguments"

	^ self selectors select: [:selector | selector numArgs = numberOfArgs]