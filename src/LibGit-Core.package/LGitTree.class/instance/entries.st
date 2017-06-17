traversing
entries
	| entryCount |
	entryCount := self tree_entrycount: self.
	
	^ Array new: entryCount streamContents: [ :stream |
		0 to: entryCount - 1 do: [ :i || entry |
			entry := self tree_entry_byindex: self idx: i.
			entry repository: self repository.
			stream nextPut: entry ] ]