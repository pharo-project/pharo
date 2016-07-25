label-styles
createExpandBoxFor: aSystemWindow
	"Answer a button for maximising/restoring the window."
	
	^aSystemWindow createBox
		labelGraphic: self windowMaximizeForm;
		extent: aSystemWindow boxExtent;
		actionSelector: #expandBoxHit;
		setBalloonText: 'Expand to full screen' translated