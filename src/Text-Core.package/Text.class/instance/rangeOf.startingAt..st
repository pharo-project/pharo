accessing
rangeOf: attribute startingAt: index
"Answer an interval that gives the range of attribute at index position  index. An empty interval with start value index is returned when the attribute is not present at position index.  "
   ^string size = 0
      ifTrue: [index to: index - 1]
	 ifFalse: [runs rangeOf: attribute startingAt: index]