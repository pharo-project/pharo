accessing
actualClass
	"Answer the class represented in the receiver."

	^ super actualClass ifNil: [ Smalltalk globals classNamed: self item ]