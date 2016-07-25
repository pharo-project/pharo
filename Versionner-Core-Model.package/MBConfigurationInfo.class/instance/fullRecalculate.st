updating
fullRecalculate

	super fullRecalculate.
	(currentVersion == #recalculate or: [ currentVersion isNil ]) ifTrue: [^self ].
	currentVersion := #recalculate.
