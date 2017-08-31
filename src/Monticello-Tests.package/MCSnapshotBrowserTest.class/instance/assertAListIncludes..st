asserting
assertAListIncludes: anArrayOfStrings
	self listMorphs 
			detect: [:m | m getList includesAll: anArrayOfStrings]
			ifNone: [self assert: false].