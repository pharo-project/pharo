private
toolbarButtonsForPresentation: aPresentation

	^ (aPresentation allActions select: [:each | each isButton ]) 
		collect: [:each | |actionMorph|
			actionMorph := (each renderGlamorouslyOn: self renderer).
			actionMorph arguments 
				ifNil: [ actionMorph arguments: { aPresentation } ]
				ifNotNil: [ actionMorph arguments: ({ aPresentation }, actionMorph arguments) ].
			actionMorph ].
	