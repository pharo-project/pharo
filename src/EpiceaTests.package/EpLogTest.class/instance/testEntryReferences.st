tests
testEntryReferences

	| entryReferences |
	entryReferences := log entryReferences.
	
	entryReferences do: [:each |
		log
			entryFor: each
			ifPresent: [:entry | "ok" ]
			ifAbsent: [ self fail ] ].

	self assert: entryReferences size equals: 5.
