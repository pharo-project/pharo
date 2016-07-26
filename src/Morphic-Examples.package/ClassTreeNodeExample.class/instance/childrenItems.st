menu
childrenItems
	^ (self item subclasses asArray sort: [:a :b | a name < b name]) asOrderedCollection
