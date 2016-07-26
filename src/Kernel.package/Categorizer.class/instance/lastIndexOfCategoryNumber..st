private
lastIndexOfCategoryNumber: anInteger
	anInteger > categoryStops size ifTrue: [^ nil].
	^ categoryStops at: anInteger