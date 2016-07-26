label-styles
configureWindowLabelAreaFor: aWindow
	"Configure the label area for the given window."

	aWindow labelArea
		addMorphBack: (Morph new extent: aWindow class borderWidth @ 0).
	aWindow hasCloseBox ifTrue: [aWindow addCloseBox].
	aWindow hasMenuBox ifTrue: [aWindow addMenuControl].
	aWindow labelArea
		addMorphBack: (Morph new extent: aWindow class borderWidth @ 0).
	aWindow basicLabel ifNotNil: [:label |
		label hResizing: #spaceFill.
		aWindow labelArea addMorphBack: label].
	aWindow hasExpandBox ifTrue: [aWindow addExpandBox].
	aWindow hasCollapseBox ifTrue: [aWindow addCollapseBox].
	aWindow labelArea
		addMorphBack: (Morph new extent: aWindow class borderWidth @ 0)