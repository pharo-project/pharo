label-styles
createCollapseBoxFor: aSystemWindow
	"Answer a button for minimising the window."
	
	^aSystemWindow createBox
		labelGraphic: self windowMinimizeForm;
		extent: aSystemWindow boxExtent;
		actionSelector: #collapseBoxHit;
		setBalloonText: 'Collapse this window' translated