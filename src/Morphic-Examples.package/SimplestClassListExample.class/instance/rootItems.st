accessing
rootItems
	^ rootClass allSubclasses asArray sort: [:a :b | a name < b name]