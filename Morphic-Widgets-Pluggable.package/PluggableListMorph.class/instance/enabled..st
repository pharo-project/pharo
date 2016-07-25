configuration
enabled: aBoolean
	"Set the enablement state of the receiver."

	aBoolean = self enabled
		ifFalse: [ enabled :=  aBoolean.
				self adoptPaneColor: self paneColor;
					changed]