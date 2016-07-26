theme
themeChanged

	self
	layoutInset: (self theme buttonLabelInsetFor: self);
	cornerStyle: (self theme buttonCornerStyleIn: self window);
	fillStyle: self themedFillStyle.
	
	label ifNotNil: [ label color: self fillStyle asColor contrastingColor ].

	super themeChanged