initialization
addCloseBox
	"If I have a labelArea, add a close box to it"
	| frame |
	labelArea
		ifNil: [^ self].
	closeBox := self createCloseBox.
	frame := LayoutFrame identity leftOffset: 2.
	closeBox layoutFrame: frame.
	labelArea addMorphBack: closeBox