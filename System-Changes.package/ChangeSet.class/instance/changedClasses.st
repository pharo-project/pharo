class changes
changedClasses
	"Answer an OrderedCollection of changed or edited classes.
	Does not include removed classes.  Sort alphabetically by name."

	"Much faster to sort names first, then convert back to classes.  Because metaclasses reconstruct their name at every comparison in the sorted collection.
	8/91 sw chgd to filter out non-existent classes (triggered by problems with class-renames"

	^ self changedClassNames
		collect: [ :className | Smalltalk globals classNamed: className ]
		thenSelect: [ :aClass | aClass notNil ]