private
useExtraSelection
	^ self useFindReplaceSelection or: [self useSecondarySelection]
