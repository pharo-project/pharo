testing
accepts: anOmEntry

	| reference |
	reference := theLog referenceTo: anOmEntry.

	^ (rejectedEntryReferences includes: reference) not