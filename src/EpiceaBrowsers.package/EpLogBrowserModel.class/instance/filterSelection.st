menu - filters
filterSelection

	| references |
	references := self selectedEntryReferences.

	references ifEmpty: [ ^self inform: 'Selection is empty' ].

	self addFilter: (EpEntryReferencesFilter new
		theLog: log; 
		rejectedEntryReferences: references; 
		yourself).
