accessing
layoutOfPaneNamed: aSymbol
	^ self panesLayouts
		at: aSymbol
		ifAbsent:
			[ | index pas yTop yBot |
			"I think this can be clean a little but I don't have the time right now."
			index := (self panes collect: [ :e | e name ]) indexOf: aSymbol.
			pas := 1 / ((self panes size / 2) floor + 1).
			yTop := ((index / 2) rounded - 1) * pas.
			yBot := (index / 2) rounded * pas.
			(index = self panes size and: [ index odd ])
				ifTrue: [ ^ (0 @ yTop corner: 1 @ yBot) asLayoutFrame ].
			index odd
				ifTrue: [ (0 @ yTop corner: 0.5 @ yBot) asLayoutFrame ]
				ifFalse: [ (0.5 @ yTop corner: 1 @ yBot) asLayoutFrame ] ]