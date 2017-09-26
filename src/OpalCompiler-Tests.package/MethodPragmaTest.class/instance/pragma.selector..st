utilities
pragma: aString selector: aSelector
	^ (self compile: '<' , aString , '>' selector: aSelector)
		pragmas first.