theme
themeChanged
	"Update the on image."

	self onImage: (self isRadioButton
		ifTrue: [self theme radioButtonMarkerForm]
		ifFalse: [self theme checkboxMarkerForm]).
	self adoptPaneColor: self paneColor.
	super themeChanged