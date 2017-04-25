accessing
named: aGlobalName inDirectory: aDirectory

	| fileReference |
	fileReference := OmFileStore fileReferenceForStoreNamed: aGlobalName inDirectory: aDirectory.

	^ storeByPath
		at: fileReference fullName
		ifPresent: [:store | store ]
		ifAbsentOrNil: [ | newStore |
			newStore := self newStoreNamed: aGlobalName in: aDirectory.
			storeByPath at: fileReference fullName put: newStore.
			newStore ]
