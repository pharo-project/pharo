private
modelCurrentSelectedClass
	" ugly dispatch, but current Browser protocol names aren't really cool "
	" for Nautilus ( and other tools), selectedClass sounds better, but I think it shouldn't be hardcoded "
	" IT'S A TEMP SOLUTION -- BenjaminVanRyseghem 14/04/11"

	" From now, all tools should use #selectedBehavior, waiting for a better solution - Thx Nice "
	(model respondsTo: #selectedBehavior)
		ifTrue: [^ model selectedBehavior ].
	
	" old selectors in order of uses "
	(model respondsTo: #selectedClass)
		ifTrue: [^ model selectedClass ].
	(model respondsTo: #selectedClassOrMetaClass)
		ifTrue: [ ^ model selectedClassOrMetaClass ].
	
		
	^ nil