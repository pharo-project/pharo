announcement
allKeystrokeActions
	^ self model notNil 
		ifTrue: [
			(self model allActions, self model allSelectionActions) select: [ :action | 
				action hasShortcut ]]
		ifFalse: [#()]