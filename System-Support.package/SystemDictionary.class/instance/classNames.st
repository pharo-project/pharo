class and trait names
classNames
	"Answer a sorted collection of all class names. Use the return value of #fillCaches to avoid concurrency issues."
	
	^cachedClassNames ifNil: [ self fillCaches at: 1 ]