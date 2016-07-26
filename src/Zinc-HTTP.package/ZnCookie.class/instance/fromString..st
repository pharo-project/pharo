initialize-release
fromString: aString
	| tokens val data i |
	tokens := aString substrings: ';'.
	i := tokens first indexOf: $=.
	val := tokens first copyFrom: 1 to: i - 1.
	data := tokens first copyFrom: i + 1 to: tokens first size. 
	self name: val trimBoth; value: data trimBoth.
	2 to: tokens size do: [ :index | 
		self readAttribute: (tokens at: index) ]