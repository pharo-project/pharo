accessing
rootItems
	^ self rootClass allSubclasses asArray sort: [:a :b | a name < b name ]