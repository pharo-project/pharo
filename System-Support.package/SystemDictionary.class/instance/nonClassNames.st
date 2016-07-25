class and trait names
nonClassNames
	"Answer a sorted collection of all non-class names. Use the return value of #fillCaches to avoid concurrency issues."
	
	^cachedNonClassNames ifNil: [ self fillCaches at: 2 ]