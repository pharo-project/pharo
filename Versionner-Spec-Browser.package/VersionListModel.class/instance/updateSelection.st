updating
updateSelection
	selectionVersionString ifNil: [ ^self ].
	self listItems 
		detect: [ :item | item versionString = selectionVersionString ]
		ifFound: [ :item | | index |
				 index := self listItems identityIndexOf: item.
				selectionHolder index value: index.
				selectionHolder selection value: item ]
		ifNone: [ self resetSelection ]