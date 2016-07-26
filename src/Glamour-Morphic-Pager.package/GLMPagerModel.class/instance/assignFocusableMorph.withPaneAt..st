focus
assignFocusableMorph: aMorph withPaneAt: paneIndex
	"assigns previosely focused morph with corresponding pane"
	
	focusedMorphs at: paneIndex put: aMorph.