column handling
columns
	"Return a column definitions, if empty, return a collection with one column"
	^columns ifNil: [self columns: (OrderedCollection with: MorphTreeColumn new). columns]