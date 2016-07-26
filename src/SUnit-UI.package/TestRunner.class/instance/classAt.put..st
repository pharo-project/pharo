accessing-classes
classAt: anInteger put: aBoolean
	classesSelected := classesSelected
		perform: (aBoolean ifTrue: [ #copyWith: ] ifFalse: [ #copyWithout: ])
		with: (classes at: anInteger ifAbsent: [ ^ self ]).
	self changed: #classSelected; changed: #hasRunnable.