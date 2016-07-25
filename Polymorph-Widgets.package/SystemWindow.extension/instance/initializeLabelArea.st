*Polymorph-Widgets
initializeLabelArea
	"Initialize the label area (titlebar) for the window."
	
	label := self theme windowLabelFor: self.
	"Add default inital boxes"
	collapseBox := self createCollapseBox. "Add collapse box so #labelHeight will work"
	closeBox := self createCloseBox.
	self wantsExpandBox ifTrue: [
		expandBox := self createExpandBox.
		self setExpandBoxBalloonText].
	menuBox := self createMenuBox.
	stripes := Array
						with: (Morph newBounds: bounds)
						with: (Morph newBounds: bounds).
	self addLabelArea.
	labelArea
		goBehind.
	self replaceBoxes.
	labelArea fillStyle: self activeTitleFillStyle.