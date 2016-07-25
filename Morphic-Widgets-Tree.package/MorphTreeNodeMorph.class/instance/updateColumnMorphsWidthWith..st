updating
updateColumnMorphsWidthWith: aListOfWidths
	| sw |
	(container columns isEmpty or: [self hasSubmorphs not]) ifTrue: [^ self].
	sw := self spacerWidth.
	1 to: aListOfWidths size - 1 
		do: [:idx | | w | 
			w := aListOfWidths at: idx.
			(controls at: idx) width: (w - (idx = 1 ifTrue: [sw] ifFalse: [0]))].
