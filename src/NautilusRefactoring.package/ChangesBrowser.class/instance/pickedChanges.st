accessing
pickedChanges
	| selectedItems |
	selectedItems := changesTree selectedItems collect: #content.
	^ changesTree roots select: [ :i | selectedItems includes: i ]