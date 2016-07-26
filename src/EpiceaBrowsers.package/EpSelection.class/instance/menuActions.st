populating menu
menuActions

	| items |
	items := self itemsForMenuAction.

	items ifEmpty: [ ^ #() ].

	^ (items
		collect: [ :each | each menuActions ])
		reduce: [ :a :b | a intersection: b ]