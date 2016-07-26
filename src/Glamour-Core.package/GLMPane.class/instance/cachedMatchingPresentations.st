accessing
cachedMatchingPresentations
	^ cachedMatchingPresentations ifNil: [ cachedMatchingPresentations := self matchingPresentations ]