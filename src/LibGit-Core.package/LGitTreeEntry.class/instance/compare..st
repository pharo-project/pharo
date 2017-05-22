private
compare: aTreeEntry
	^ (self
		tree_entry_cmp: self
		e2: aTreeEntry) isZero