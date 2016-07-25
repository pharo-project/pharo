tests
testCachedMatchingPresentations
	| aPane cachedMatchingPresentations |
	aPane := GLMPane in: nil.
	cachedMatchingPresentations := aPane cachedMatchingPresentations.
	self assert: cachedMatchingPresentations == aPane cachedMatchingPresentations.
	aPane matchingPresentations "this should refresh cached matching presentations".
	self assert: cachedMatchingPresentations ~~ aPane cachedMatchingPresentations