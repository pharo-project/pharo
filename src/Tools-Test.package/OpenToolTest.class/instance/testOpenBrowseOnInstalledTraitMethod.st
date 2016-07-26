test browse
testOpenBrowseOnInstalledTraitMethod
	| browser |
	browser := (Class>>#hasClassSide) browse.
	browser changed.
	browser close.