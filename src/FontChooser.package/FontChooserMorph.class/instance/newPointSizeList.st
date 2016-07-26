interface building
newPointSizeList 

	| widget |
	widget := PluggableListMorph
		on: self model
		list: #pointSizeList
		selected: #selectedPointSizeIndex
		changeSelected: #selectedPointSizeIndex:.
	^ widget	
			color: Color white;
			borderInset;
			vResizing: #spaceFill;
			hResizing: #spaceFill;
			yourself.