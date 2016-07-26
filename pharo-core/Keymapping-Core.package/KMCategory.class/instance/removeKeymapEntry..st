associating
removeKeymapEntry: aKeymapEntry

	[ self commonEntries remove: aKeymapEntry ] 
	on: Error
	do: [ self platformEntries remove: aKeymapEntry ]