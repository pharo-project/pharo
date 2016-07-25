initialization
initialize: aCapacity
	dictionary := self dictionaryClass new: aCapacity.
	orderedKeys := Array new: aCapacity.