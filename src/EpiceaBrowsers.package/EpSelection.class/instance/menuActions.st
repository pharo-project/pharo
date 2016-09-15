populating menu
menuActions

	| items |
	items := self selectedEntryItems.

	items ifEmpty: [ ^ #() ].

	^ (items
		collect: [ :each | each menuActions ])
		reduce: [ :a :b | a intersection: b ]