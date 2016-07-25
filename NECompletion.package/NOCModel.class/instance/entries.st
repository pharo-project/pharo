accessing
entries
	entries ifEmpty: [ self loadEntries ].
	^  entries