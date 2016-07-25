accessing
rootItems
	^ self resultDictionary keys sort: [ :a :b | a asString < b asString ]