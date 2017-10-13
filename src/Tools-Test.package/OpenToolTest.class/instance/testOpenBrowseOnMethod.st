test browse
testOpenBrowseOnMethod
	| browser |
	browser := (Object>>#yourself) browse.
	browser changed.
	browser close.