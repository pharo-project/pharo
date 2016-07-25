accessing
rootItems
	^ self itemsForTest asArray sort: [:a :b | a name < b name]