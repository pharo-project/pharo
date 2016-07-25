class changes
changedClassNames
	"Answer a OrderedCollection of the names of changed or edited classes.
	DOES include removed classes.  Sort alphabetically."

	^ changeRecords keysSortedSafely 