private
modelCurrentSelectedClass
	" ugly dispatch, but current Browser protocol names aren't really cool "
	" for Nautilus ( and other tools), selectedClass sounds better, but I think it shouldn't be hardcoded "
	" IT'S A TEMP SOLUTION -- BenjaminVanRyseghem 14/04/11"

	" From now, all tools should use #selectedBehavior, waiting for a better solution - Thx Nice "
	(self model interactionModel respondsTo: #selectedBehavior)
		ifTrue: [^ self model interactionModel selectedBehavior ].
	
	" old selectors in order of uses "
	(self model respondsTo: #selectedClass)
		ifTrue: [^ self model selectedClass ].
	(self model respondsTo: #selectedClassOrMetaClass)
		ifTrue: [ ^ self model selectedClassOrMetaClass ].
	
		
	^ nil