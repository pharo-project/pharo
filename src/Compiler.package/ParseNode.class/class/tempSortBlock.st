accessing
tempSortBlock
	"Answer a block that can sort a set of temporaries into a stable
	 order so that different compilations produce the same results."
	^[:t1 :t2| | be1 be2 bs1 bs2 |
	   t1 index < t2 index "simple sort by index."
	   or: [t1 index = t2 index "complex tie break" 
		  and: [t1 isRemote ~= t2 isRemote
				ifTrue: [t2 isRemote] "put direct temps before indirect temps"
				ifFalse: 
					[((be1 := t1 definingScope blockExtent) isNil
					  or: [(be2 := t2 definingScope blockExtent) isNil])
						ifTrue: [t1 name < t2 name] "only have the name left to go on"
						ifFalse: "put temps from outer scopes before those from inner scopes"
							[(bs1 := be1 first) < (bs2 := be2 first)
							 or: [bs1 = bs2 and: [t1 name < t2 name]]]]]]] "only have the name left to go on"