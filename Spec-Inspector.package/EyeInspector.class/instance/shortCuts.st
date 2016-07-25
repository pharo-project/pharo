initialization
shortCuts
	| shortCuts |
	shortCuts := super shortCuts.

	shortCuts at: $c meta put: [ self copySelectedItemAccessorCode ].
	
	^ shortCuts