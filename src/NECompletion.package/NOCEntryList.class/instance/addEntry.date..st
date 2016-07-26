adding
addEntry: aSymbol date: aDate

	| entry currentEntry |
	entry := entries at: aSymbol ifAbsent: nil.
	entry 
		ifNotNil: [ entry unlink ]
		ifNil: [ 
			entry := NOCDatedEntry contents: aSymbol type: #selector.
			entries at: aSymbol put: entry ].
	entry date: aDate.
	currentEntry := entryHead previous.
	[ currentEntry == entryHead or: [
		currentEntry date <= aDate ] ] whileFalse: [
			currentEntry := currentEntry previous ].
	currentEntry link: entry.
	entries size > size ifTrue: [
		entry := entryHead next unlink.
		entries removeKey: entry contents ]