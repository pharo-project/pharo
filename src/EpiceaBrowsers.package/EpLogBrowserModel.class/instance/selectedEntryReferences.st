menu - accessing
selectedEntryReferences
	self flag: #todo. "Workaround: FastTableModel can return nil"

	^ itemsModel selectedItemsSorted select: [:each | each isNotNil ]