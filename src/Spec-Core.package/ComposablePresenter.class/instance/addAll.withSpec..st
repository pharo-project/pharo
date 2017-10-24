private
addAll: aWindow withSpec: aSpec

	aWindow
		addMorph: (self buildWithSpec: aSpec)
		frame: (0@0 corner: 1@1).