private
askForSuperclassOf: aClass to: aSuperclass toImplement: aSelector ifCancel: cancelBlock
	| classes currentSuperclass chosenClassIndex |
	
	classes := OrderedCollection new.
	classes add: aClass.
	currentSuperclass := aClass superclass.
	[ currentSuperclass == nil or: [ currentSuperclass == aSuperclass ] ]
		whileFalse: [
			classes add: currentSuperclass.
			currentSuperclass := currentSuperclass superclass ].
	classes := classes addAll: (aClass traits sort: [ :t1 :t2 | t1 asString < t2 asString ]); yourself.
	
	classes size = 1 ifTrue: [ ^ classes at: 1 ].
	chosenClassIndex := UIManager default 
		chooseFrom: (classes collect: [:c | c name])
		title: 'Define #', aSelector, ' in which class?'.
	chosenClassIndex = 0 ifTrue: [^ cancelBlock value].
	^ classes at: chosenClassIndex