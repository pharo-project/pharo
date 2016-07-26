private
checkCodeSize
	(freeCode > maxCode and: [ codeSize < 12 ]) ifTrue: 
		[ codeSize := codeSize + 1.
		maxCode := (1 bitShift: codeSize) - 1 ]