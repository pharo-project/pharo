actions
enabled: aBoolean
	"Set the value of enabled"

	enabled = aBoolean ifTrue: [^self].
	enabled := aBoolean.
	self changed: #enabled.
	self
		adoptPaneColor: self paneColor;
		changed