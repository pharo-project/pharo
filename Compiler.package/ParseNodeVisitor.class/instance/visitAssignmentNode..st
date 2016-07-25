visiting
visitAssignmentNode: anAssignmentNode
	"N.B.  since assigment happens after the value is evaluated the value is visited first."
	anAssignmentNode value accept: self.
	anAssignmentNode variable accept: self