initialization
initialize
	super initialize.
	self matchesAny: #(
		'(`@collectionOwner `@collectionGetter: `@args) add:           `@arg'
		'(`@collectionOwner `@collectionGetter: `@args) remove:     `@arg'
		'(`@collectionOwner `@collectionGetter: `@args) addAll:       `@arg'
		'(`@collectionOwner `@collectionGetter: `@args) removeAll: `@arg'
		)