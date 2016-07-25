accessing
enabled: anObject
	"Set the value of enabled"

	enabled = anObject ifTrue: [^self].
	enabled := anObject.
	anObject ifFalse: [self hideList].
	self changed: #enabled.
	self
		adoptPaneColor: self paneColor;
		changed