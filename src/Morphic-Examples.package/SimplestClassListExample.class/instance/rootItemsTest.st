accessing
rootItemsTest
	^ (rootClass allSubclasses asArray sort: [:a :b | a name < b name])