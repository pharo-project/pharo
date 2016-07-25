layout-properties
layoutProperties: newProperties
	"Return the current layout properties associated with the receiver"
	self layoutProperties == newProperties ifTrue:[^self].
	self assureExtension layoutProperties: newProperties.
