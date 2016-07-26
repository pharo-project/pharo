*Polymorph-Widgets
addMenuControl
	"If I have a label area, add a menu control to it."

	labelArea ifNil: [^ self]. "No menu if no label area"
	menuBox
		ifNotNil: [menuBox delete].
	labelArea addMorphBack: (menuBox := self createMenuBox)