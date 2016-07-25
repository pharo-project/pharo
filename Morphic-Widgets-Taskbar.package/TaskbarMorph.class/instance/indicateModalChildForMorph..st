taskbar
indicateModalChildForMorph: aMorph
	"Flash the button corresonding to the given morph ."
	
	(self buttonForMorph: aMorph) ifNotNil: [:b |
		b indicateModalChild]