*glamour-morphic-renderer
installKeyCombinationForPresentation: aPresentation onMorph: aMorph
	aMorph 
		bindKeyCombination: self keymap		
		toAction: [ self actOn: aPresentation ].
	