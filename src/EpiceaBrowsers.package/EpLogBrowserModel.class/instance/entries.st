accessing
entries

	^ log entryReferences collect: [:each | log entryFor: each ]
