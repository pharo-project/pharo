A SharedPackageFilterPatternsHolder is used for package filter input. It shares historical values of the input among all the PackageTreeNautilus browsers. Variable lastValue keeps the last used value. It is used when a browser is opened from a system (e.g. World menu, anObject browse).

Instance Variables
	mutex:		Mutex
	value:		OrderedCollection
	lastValue:	String
