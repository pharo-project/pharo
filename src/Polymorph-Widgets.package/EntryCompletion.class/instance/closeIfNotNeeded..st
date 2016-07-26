event-handling
closeIfNotNeeded: aMorph

	(chooser isNotNil and:[ chooser hasKeyboardFocus or: [ aMorph hasKeyboardFocus ] ]) ifFalse: [ self closeChooser ]