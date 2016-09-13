removing
keysAndValuesRemove: keyValueBlock
	"Removes all entries for which keyValueBlock returns true."
	"When removing many items, you must not do it while iterating over the dictionary, since it may be changing.  This method takes care of tallying the removals in a first pass, and then performing all the deletions afterward.  Many places in the sytem could be simplified by using this method."

	| removals |
	removals := OrderedCollection new.
	self keysAndValuesDo:
		[:key :value | (keyValueBlock value:  key value:  value)
			ifTrue: [removals add:  key]].
 	removals do:
		[:aKey | self removeKey: aKey]