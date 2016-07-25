merging
mergeExisting: existingDictionary stored: storedDictionary
	^ GlobalIdentifierMerger new
			existing: existingDictionary;
			stored: storedDictionary;
			merge