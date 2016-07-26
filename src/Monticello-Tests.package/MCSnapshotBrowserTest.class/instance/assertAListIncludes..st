asserting
assertAListIncludes: anArrayOfStrings
	self listMorphs 
			detect: [:m | m getList includesAllOf: anArrayOfStrings]
			ifNone: [self assert: false].