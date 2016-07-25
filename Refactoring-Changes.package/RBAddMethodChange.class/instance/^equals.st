comparing
= anAddMethodChange
	super = anAddMethodChange
		ifFalse: [ ^ false ].
	^ self parseTree = anAddMethodChange parseTree