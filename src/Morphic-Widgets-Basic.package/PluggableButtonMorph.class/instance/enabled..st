accessing
enabled: aBoolean
	"Set the enabled state of the receiver."
	
	enabled = aBoolean ifTrue: [^self].
	enabled := aBoolean.
	self updateLabelEnablement.
	self changed