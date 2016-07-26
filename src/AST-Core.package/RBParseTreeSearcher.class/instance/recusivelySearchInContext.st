private
recusivelySearchInContext
	"We need to save the matched context since the other searches might overwrite it."

	| oldContext |
	oldContext := context.
	context := SmallDictionary new.
	self lookForMoreMatchesInContext: oldContext.
	context := oldContext