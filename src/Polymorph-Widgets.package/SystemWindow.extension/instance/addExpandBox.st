*Polymorph-Widgets
addExpandBox
	"If I have a labelArea, add a close box to it"
	labelArea
		ifNil: [^ self].
	expandBox := self createExpandBox.
	self setExpandBoxBalloonText.
	expandBox layoutFrame: (LayoutFrame new leftFraction: 1; leftOffset: (self boxExtent x * 2 + 3) negated).
	labelArea addMorphBack: expandBox