private
resolveReference: reference
	unresolvedReferences := unresolvedReferences - 1.
	^ self resolveReferenceIndex: reference index