*Polymorph-Widgets
addCollapseBox
	"If I have a labelArea, add a collapse box to it."
	
	labelArea
		ifNil: [^ self].
	collapseBox := self createCollapseBox.
	collapseBox layoutFrame: (self theme windowCollapseBoxLayoutFor: self).
	labelArea addMorphBack: collapseBox