add-remove
removeAllForType: typeFlag
	| toRemove |
	
	toRemove := IdentitySet new.
	fifo do:[:entry |
		entry type = typeFlag
			ifTrue:[toRemove add: entry]].
	toRemove do:[:entry | | d | 
		fifo remove: entry.
		d := (fontTable at: entry font) at: entry charCode.
		d removeKey: entry type.	
		used := used - (self sizeOf: entry object) ]. 

	