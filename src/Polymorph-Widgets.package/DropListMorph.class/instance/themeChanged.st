updating
themeChanged
	"Update the selection colour."
	
	self color: self defaultColor.
	self selectionColor ifNotNil: [
		self selectionColor: self theme selectionColor].
	self layoutInset: self layoutInsetToUse.
	self buttonMorph extent: self buttonExtent.
	self buttonMorph cornerStyle: self cornerStyle.
	self buttonMorph label: self buttonLabel.
	self updateContentColor: nil.
	self listMorph themeChanged.
	super themeChanged.	
	