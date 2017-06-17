asserting
assertAListMatches: strings
	| lists |
	lists := self listMorphs collect: #getList.
	lists
		detect: [ :list|
			(list size = strings size) and: [list includesAll: strings]]
		ifNone: [ 
			self fail: 'Could not find all "', strings asArray asString, '" ', 
						'in any of "', (lists collect: #asArray) asArray asString, '"'  ].