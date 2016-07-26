accessing - properties
setProperties: aList
	"Set many properties at once from a list of prop, value, prop, value"

	1 to: aList size by: 2 do: [:ii |
		self setProperty: (aList at: ii) toValue: (aList at: ii+1)].