update
adoptGradientColor: aColor
	"Adopt the given pane color."

	|c fs bfs bs bbs|
	aColor ifNil:[^self].
	c := aColor.
	fs := self normalThumbFillStyle.
	bfs := self normalButtonFillStyle.
	bs := self normalThumbBorderStyle.
	bbs := self normalButtonBorderStyle.
	sliderColor := c.
	downButton
		fillStyle: bfs;
		borderStyle: bbs.
	upButton
		fillStyle: bfs shallowCopy;
		borderStyle: bbs.
	slider
		fillStyle: fs;
		borderStyle: bs.
	self updateUpButtonImage.
	self updateDownButtonImage