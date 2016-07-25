*Polymorph-Widgets
setWindowColor: incomingColor
	"Removed existing color check - looked useless!"
	
	| aColor |
	incomingColor ifNil: [^ self].  "it happens"
	aColor := incomingColor.
	self setProperty: #paneColor toValue: aColor.
	self setStripeColorsFrom: aColor.
	self theme settings fadedBackgroundWindows 
		ifFalse: [self adoptPaneColor: aColor]. "reverse optimisation"
	self changed.