label-styles
createMenuBoxFor: aSystemWindow
	"Answer a button for the window menu."
	
	^aSystemWindow createBox
		labelGraphic: (self windowMenuIconFor: aSystemWindow);
		extent: aSystemWindow boxExtent;
		actWhen: #buttonDown;
		actionSelector: #offerWindowMenu;
		setBalloonText: 'Window menu' translated