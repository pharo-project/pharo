rendering
renderToolbarForPresentation: aPresentation inMorph: aMorph
	| dock buttons |
	dock := aMorph theme newToolDockingBarIn: aMorph.
	buttons := self toolbarButtonsForPresentation: aPresentation.
	dock addAllMorphs: buttons.
	dock addMorphBack: (self 
		dropDownMenuFrom: [ | menu defaultMenu |
				defaultMenu := aMorph buildWindowMenu.
				(aPresentation allActions select: #isMenu) ifNotEmpty: [ 
					defaultMenu addLine ].
				menu := self menuIn: defaultMenu for: aPresentation.
				menu popUpEvent: self currentEvent in: aMorph world ]
		withIcon: aMorph theme windowMenuForm).
	^ dock