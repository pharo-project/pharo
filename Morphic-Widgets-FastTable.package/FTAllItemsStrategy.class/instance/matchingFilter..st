action
matchingFilter: anItem
	((anItem children collect: [ :it | self matchingFilter: it ]) reject: #isNil)
		ifNotEmpty:
			[ :coll | 
			^ (self newStaticItemFrom: anItem)
				children: coll asArray;
				dataSource: anItem dataSource;
				expandWithoutChange;
				yourself ].
	^ (pattern matchesIn: (self dataSource toString: anItem)) ifEmpty: [ nil ] ifNotEmpty: [ self newStaticItemFrom: anItem ]