private
find: oldObject
  "  This method answers an index in the range firstIndex .. lastIndex, which is meant for internal use only.
     Never use this method in your code, the methods for public use are:
        #indexOf:
        #indexOf:ifAbsent: "

	| index |
	index := firstIndex.
	[index <= lastIndex]
		whileTrue:
			[(array at: index) = oldObject ifTrue: [^ index].
			index := index + 1].
	self errorNotFound: oldObject