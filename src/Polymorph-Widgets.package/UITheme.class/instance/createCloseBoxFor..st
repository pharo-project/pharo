label-styles
createCloseBoxFor: aSystemWindow
	"Answer a button for closing the window."
	
	^(self
		newCloseControlIn: aSystemWindow
		for: aSystemWindow
		action: [aSystemWindow closeBoxHit]
		help: 'Close this window' translated)
		extent: aSystemWindow boxExtent