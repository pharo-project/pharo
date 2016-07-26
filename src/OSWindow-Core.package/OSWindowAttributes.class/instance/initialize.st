initialization
initialize
	visible := self class defaultVisible.
	position := self class defaultPosition.
	extent := self class defaultExtent.
	fullscreen := self class defaultFullscreen.
	title := self class defaultTitle.
	windowCentered := self class defaultWindowCentered.
	
	borderless := self class defaultBorderless.
	resizable := self class defaultResizable.
	minimized := self class defaultMinimized.
	maximized := self class defaultMaximized.
	
	glAttributes := nil.