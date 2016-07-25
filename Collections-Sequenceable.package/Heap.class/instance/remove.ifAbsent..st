removing
remove: oldObject ifAbsent: aBlock
	"Remove oldObject as one of the receiver's elements. If several of the 
	elements are equal to oldObject, only one is removed. If no element is 
	equal to oldObject, answer the result of evaluating anExceptionBlock. 
	Otherwise, answer the argument, oldObject."
	1 to: tally do:[:i| 
		(array at: i) = oldObject ifTrue:[^self privateRemoveAt: i]].
	^aBlock value