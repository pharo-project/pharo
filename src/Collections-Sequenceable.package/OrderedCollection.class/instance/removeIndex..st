private
removeIndex: removedIndex
  "  removedIndex is an index in the range firstIndex .. lastIndex, such an index is not known from outside the collection.
    Never use this method in your code, it is meant for private use by OrderedCollection only.
     The method for public use is:
        #removeAt: "

	array 
		replaceFrom: removedIndex 
		to: lastIndex - 1 
		with: array 
		startingAt: removedIndex+1.
	array at: lastIndex put: nil.
	lastIndex := lastIndex - 1.