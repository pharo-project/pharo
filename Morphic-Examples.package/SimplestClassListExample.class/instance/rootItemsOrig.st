as yet unclassified
rootItemsOrig
	^ rootClass allSubclasses asArray sort: [:a :b | a name < b name]