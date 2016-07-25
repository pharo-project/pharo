private
computeHeight
	^self isLeaf
		ifTrue:[height := 0]
		ifFalse:[height := (left computeHeight max: right computeHeight) + 1].