accessing
objectCount: aNumber

	objectCount := aNumber.
	objectsIndexes := FLLargeIdentityDictionary new.
	substitutionIndexes := FLLargeIdentityDictionary new.
	indexStream := FLIndexStream on: stream digits: aNumber digitLength