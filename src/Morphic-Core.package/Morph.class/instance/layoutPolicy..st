layout-properties
layoutPolicy: aLayoutPolicy
	"Layout specific. Return the layout policy describing how children of the receiver should appear."
	self layoutPolicy == aLayoutPolicy ifTrue:[^self].
	self assureExtension layoutPolicy: aLayoutPolicy.
	self layoutChanged.