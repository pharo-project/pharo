test browse
testOpenBrowseOnTraitMethod
	| browser |
	browser := (TBehavior>>#name) browse.
	browser changed.
	browser close.