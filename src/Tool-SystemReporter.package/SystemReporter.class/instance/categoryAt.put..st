accessing-categories
categoryAt: anInteger put: aBoolean
	categoriesSelected := categoriesSelected
		perform: (aBoolean ifTrue: [ #copyWith: ] ifFalse: [ #copyWithout: ])
		with: (self categoryList at: anInteger ifAbsent: [ ^ self ]).
	self updateReport