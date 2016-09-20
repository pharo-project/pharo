*Polymorph-Widgets
themeChanged
	"Update the window colour and control boxes."

	self labelArea delete.
	self removeGrips.
	self theme
		configureWindowBorderFor: self;
		configureWindowDropShadowFor: self.
	self paneColor: self defaultBackgroundColor.
	label ifNotNil: [ "don't if label area removed"
		self initializeLabelArea].
	self setStripeColorsFrom: self paneColor.
	(self isCollapsed not or: [self isTaskbarPresent]) ifTrue: [
		self addGripsIfWanted].
	
	self isEmbedded ifTrue: [ self borderWidth: 0 ].
	
	super themeChanged