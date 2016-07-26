layout-properties
layoutInset
	"Return the extra inset for layouts"

	^ self layoutProperties ifNil: [ 0 ] ifNotNil: [ :props | props layoutInset ]