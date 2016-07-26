private
renderToolbarForPresentation: aPresentation inMorph: aMorph
	| dock buttons |
	
	dock := aMorph theme newToolDockingBarIn: aMorph.
	buttons := self toolbarButtonsForPresentation: aPresentation.
	dock addAllMorphs: buttons.
	(aPresentation allActions select: #isMenu) ifNotEmpty: [
			dock addMorphBack: (self 
				dropDownMenuFrom: [ | menu |
					menu := self menuIn: MenuMorph new for: aPresentation.
					menu popUpEvent: self currentEvent in: aMorph world ]
				withIcon: GLMUIThemeExtraIcons glamorousMore)].
	^ dock