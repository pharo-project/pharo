layout-properties
layoutPolicy
	"Layout specific. Return the layout policy describing how children 
	of the receiver should appear."
	^ extension ifNotNil: [ extension layoutPolicy]