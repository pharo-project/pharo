special instance creation
rowVector: aCollection
	"Create a matrix of one row having aCollection as contents"

	^self rows: 1 columns: aCollection size contents: aCollection asArray shallowCopy