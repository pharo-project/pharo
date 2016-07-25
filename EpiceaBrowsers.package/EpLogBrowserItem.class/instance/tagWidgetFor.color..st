private
tagWidgetFor: aString color: aColor

	^ PanelMorph new
		changeTableLayout;
		addMorphBack: aString asMorph;
		color: aColor;
		hResizing: #shrinkWrap;
		vResizing: #shrinkWrap;
		layoutInset: 3@0;
		yourself