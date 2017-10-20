private
addAll: aWindow withSpecLayout: aSpec

	aWindow
		addMorph: (self buildWithSpecLayout: aSpec)
		frame: (0@0 corner: 1@1).