halos and balloon help
balloonHelpTextForHandle: aHandle
	(aHandle eventHandler mouseSelectorsInclude: #addOrRemoveItems:)
		ifTrue: [^'Add items to, or remove them from, this selection.'].
	^ super balloonHelpTextForHandle: aHandle