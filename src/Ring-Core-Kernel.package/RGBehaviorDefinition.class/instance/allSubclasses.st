accessing class hierarchy
allSubclasses
	"Retrieves all the subclasses of the receiver in the chan hiearchy - value is kept as an annotation"
	"is a good idea storing this?"
	
	^self annotationNamed: self class allSubclassesKey
		ifAbsentPut:[ | collection index |
			index := 1.
			collection := OrderedCollection withAll: self subclasses.
			[index <= collection size] whileTrue: 
				[collection addAll: (collection at: index) subclasses.
				index := index + 1].
			collection ]