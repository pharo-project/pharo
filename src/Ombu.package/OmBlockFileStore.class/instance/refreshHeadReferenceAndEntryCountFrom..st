refreshing
refreshHeadReferenceAndEntryCountFrom: readStream

	entryCount := blocks last lastLocalNameAsInteger.
	headReference := self referenceToLocalName: entryCount asString.
