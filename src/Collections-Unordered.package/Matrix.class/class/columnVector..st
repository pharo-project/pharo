special instance creation
columnVector: aCollection
	"Create a matrix of one column having aCollection as contents"

	^ self rows: aCollection size columns: 1 contents: aCollection asArray shallowCopy