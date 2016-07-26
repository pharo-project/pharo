temp vector
tempVectorForTempStoringIt
	"If I am stroring a temp Vector, this method returns this victor.
	useful for analysis e.g. which variables are active in a context"
	
	| searchScope |
	self isStoringTempVector ifFalse: [^nil].
	searchScope := scope.
	
	[searchScope isInstanceScope or: [searchScope tempVectorName = name]] 
		whileFalse: [searchScope := searchScope outerScope].
	searchScope isInstanceScope 
		ifTrue: [ ^nil] "not found"
		ifFalse: [^ searchScope tempVector]
	