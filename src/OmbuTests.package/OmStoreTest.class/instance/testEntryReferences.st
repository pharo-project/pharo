tests
testEntryReferences
	| entryReferences |

	1 to: 7 do: [ :each | store newEntry: (OmEntry content: each) ].

	entryReferences := store entryReferences.
	
	entryReferences do: [:each |
		store
			entryFor: each
			ifPresent: [:entry | "ok" ]
			ifAbsent: [ self fail ] ].

	self assert: entryReferences size equals: 7. "To be sure it tests something"
