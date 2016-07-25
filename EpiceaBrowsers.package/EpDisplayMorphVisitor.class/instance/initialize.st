initialization
initialize

	super initialize.

	textColor := Smalltalk ui theme textColor.
	secondaryTextColor := textColor mixed: 0.2 with: Color gray.