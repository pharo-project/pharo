protocol
addModelIn: widgetToBuild withSpecLayout: aSpec
	
	widgetToBuild
		addMorph: (self model model buildWithSpecLayout: aSpec)
		frame: (0@0 corner: 1@1)