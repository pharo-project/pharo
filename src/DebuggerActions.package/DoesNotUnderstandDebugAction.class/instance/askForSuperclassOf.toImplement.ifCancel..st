private
askForSuperclassOf: aClass toImplement: aSelector ifCancel: cancelBlock
	| classes chosenClassIndex |
	classes := aClass withAllSuperclasses addAll: (aClass traits sort: [ :t1 :t2 | t1 asString < t2 asString ]); yourself.
	chosenClassIndex := UIManager default 
		chooseFrom: (classes collect: [:c | c name])
		title: 'Define #', aSelector, ' in which class?'.
	chosenClassIndex = 0 ifTrue: [^ cancelBlock value].
	^ classes at: chosenClassIndex