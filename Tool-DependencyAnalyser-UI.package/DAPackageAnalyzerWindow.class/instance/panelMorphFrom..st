adding
panelMorphFrom: aSpecWindow
	aSpecWindow submorphs
		do: [ :submorph | 
			(submorph isKindOf: PanelMorph)
				ifTrue: [ ^ submorph ] ].
	^ nil