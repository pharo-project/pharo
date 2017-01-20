enumerating
check: aClass forCritiquesDo: aCriticBlock
	| methods |
	
	methods := aClass withAllSubclasses flatCollect: #methods.
	
	aClass slots
		select: [ :slot |
			| read written |
			read := written := false.
			methods noneSatisfy: [ :method |
				read := read or: [ method readsSlot: slot ].
				written := written or: [ method writesSlot: slot ].
				read and: [ written ] ] ]
		thenDo: [ :slot |
			aCriticBlock cull:
				(self critiqueFor: aClass about: slot) ]