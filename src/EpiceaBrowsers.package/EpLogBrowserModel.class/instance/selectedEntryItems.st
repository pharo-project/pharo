menu - accessing
selectedEntryItems

	^ self selectedEntryReferences collect: [:each |
		self itemForEntryReference: each ]