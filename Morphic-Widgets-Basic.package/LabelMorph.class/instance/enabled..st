accessing
enabled: aBoolean
	"Set the value of enabled"

	enabled == aBoolean ifFalse: [
		enabled := aBoolean.
		self
			changed: #enabled;
			changed]