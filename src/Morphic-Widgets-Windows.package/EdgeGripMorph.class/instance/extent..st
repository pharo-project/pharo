geometry
extent: aPoint
	"If our minor extent changes then adopt the pane colour to
	reflect any size based gradient in the theme.
	Assumes fillStyle will not change on the major extent for
	performance reasons."

	|ext|
	ext := self extent.
	super extent: aPoint.
	self isHorizontal
		ifTrue: [self extent y ~= ext y ifTrue: [
					self adoptPaneColor: self paneColor]]
		ifFalse: [self extent x ~= ext x ifTrue: [
					self adoptPaneColor: self paneColor]]
	