private
tagWidgetFor2: aString color: aColor

	^ PanelMorph new
		changeTableLayout;
		addMorphBack: aString asMorph;
		color: aColor;
		hResizing: #rigid;
		vResizing: #rigid;
		width: 16;
		height: 16;
		wrapCentering: #center;
		cellPositioning: #topCenter;
		yourself