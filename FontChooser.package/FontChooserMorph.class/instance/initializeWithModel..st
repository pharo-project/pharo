initialize-release
initializeWithModel: aFontChooser
	self 
		model: aFontChooser;
		clipSubmorphs: true;
		setLabel: self model windowTitle;
		name: 'FontChooser'.
	self model announcer when: #pointSizeListChanged send: #updatePointSizeList to: self.
	self model announcer when: #selectedFontIndex send: #updateFontIndex to: self.
	self model announcer when: #pointSizeChanged send: #updatePointSize to: self.
	self model announcer when: #selectedFontStyleIndex send: #updateStyleList to: self.
	self updatePreview