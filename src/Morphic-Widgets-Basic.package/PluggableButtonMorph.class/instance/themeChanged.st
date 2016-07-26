updating
themeChanged
	"Set the border style to thin gray in the case of going to StandardSqueak."

	self
		layoutInset: (self theme buttonLabelInsetFor: self);
		cornerStyle: (self theme buttonCornerStyleIn: self window);
		borderStyle: (self theme buttonNormalBorderStyleFor: self).
		
	(self labelMorph isNil or: [self label isMorph ]) ifFalse: [
		self label: self label font: self labelMorph font.
		self labelMorph color: self theme textColor
	].

	super themeChanged