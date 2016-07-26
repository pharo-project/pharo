accessing
entriesMatching: prefix
	
	^Array new: entries size // 4 + 1 streamContents: [ :stream |
		| entry |
		entry := self firstEntry.
		[ entry == entryHead ] whileFalse: [
			(entry matches: prefix) ifTrue: [ stream nextPut: entry ].
			entry := entry previous ] ]