test browse
testOpenBrowseOnMethod
	| browser |
	browser := (Object>>#name) browse.
	browser changed.
	browser close.