controls
newScrollPaneFor: aMorph
	"Answer a new scroll pane morph to scroll the given morph."
	
	^self theme
		newScrollPaneIn: self
		for: aMorph