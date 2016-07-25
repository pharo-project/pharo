accessing-classes
classAt: anInteger
	^ classesSelected includes: (classes at: anInteger ifAbsent: [ ^ false ]).