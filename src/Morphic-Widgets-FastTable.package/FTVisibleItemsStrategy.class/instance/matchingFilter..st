action
matchingFilter: anItem
	anItem isExpanded
		ifTrue:
			[ ((anItem children collect: [ :each | self matchingFilter: each ]) reject: #isNil)
				ifNotEmpty: [ :coll | 
					^ (self newStaticItemFrom: anItem)
						children: coll asArray;
						expandWithoutChange;
						yourself ] ].
	^ (pattern matchesIn: (self dataSource toString: anItem))
		ifNotEmpty: [ anItem isExpanded
				ifTrue: [ self newStaticItemFrom: anItem ]
				ifFalse: [ anItem ] ]
		ifEmpty: [ nil ]