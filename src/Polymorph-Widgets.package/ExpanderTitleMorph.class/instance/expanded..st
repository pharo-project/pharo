accessing
expanded: aBoolean
	"Set the value of expanded"

	expanded := aBoolean.
	self
		changed: #expanded;
		changed: #expandLabel