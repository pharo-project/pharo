protocol
addModelIn: widgetToBuild withSpec: aSpec
	
	widgetToBuild
		addMorph: (self model model buildWithSpec: aSpec)
		frame: (0@0 corner: 1@1)