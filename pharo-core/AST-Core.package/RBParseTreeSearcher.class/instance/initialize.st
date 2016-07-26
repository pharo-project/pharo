initialization
initialize
	super initialize.
	context := SmallDictionary new.
	searches := OrderedCollection new.
	argumentSearches := OrderedCollection new: 0.
	answer := nil