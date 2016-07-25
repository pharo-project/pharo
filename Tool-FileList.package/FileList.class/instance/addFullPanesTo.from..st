utility
addFullPanesTo: window from: aCollection

	aCollection do: [ :each | 
		window addMorph: each first fullFrame: each second ]