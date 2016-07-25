examples - branches
exampleIfTrueForEffectNested
	| tmp1 |
	true
		ifTrue: [ 
			tmp1 := true.
			false
				ifTrue: [ 
					tmp1 := false.
					^ tmp1 ] ]