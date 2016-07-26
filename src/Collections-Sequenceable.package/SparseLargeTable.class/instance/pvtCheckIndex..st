private
pvtCheckIndex: index 

	index isInteger ifFalse: [self errorNonIntegerIndex].
	index < 1 ifTrue: [self errorSubscriptBounds: index].
	index > size ifTrue: [self errorSubscriptBounds: index].
