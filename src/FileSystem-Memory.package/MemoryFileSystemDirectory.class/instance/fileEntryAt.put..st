accessing
fileEntryAt: aFileName put: anEntry
	^ entries 
		at: aFileName 
		ifAbsentPut: [
			self modified.
			anEntry ]