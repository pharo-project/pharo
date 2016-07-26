private
firstIndexOfCategoryNumber: anInteger
	anInteger < 1 ifTrue: [^ nil].
	^ (anInteger > 1
			ifTrue: [(categoryStops at: anInteger - 1) + 1]
			ifFalse: [1]).