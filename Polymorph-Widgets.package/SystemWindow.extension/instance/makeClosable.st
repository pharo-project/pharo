*Polymorph-Widgets
makeClosable
	"Reinstate the close box. Go via theme to maintain box order."
	
	mustNotClose := false.
	closeBox
		ifNil: [closeBox := self createCloseBox.
				self replaceBoxes]