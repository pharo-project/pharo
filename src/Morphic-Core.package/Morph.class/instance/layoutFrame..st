layout-properties
layoutFrame: aLayoutFrame
	"Layout specific. Return the layout frame describing where the receiver should appear in a proportional layout"
	self layoutFrame == aLayoutFrame ifTrue: [^self].
	self assureExtension layoutFrame: aLayoutFrame asLayoutFrame.
	self layoutChanged.