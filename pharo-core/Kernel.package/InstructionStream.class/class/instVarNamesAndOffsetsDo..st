compiling
instVarNamesAndOffsetsDo: aBinaryBlock
	"This is part of the interface between the compiler and a class's instance or field names.
	 We override here to arrange that the compiler will use MaybeContextInstanceVariableNodes
	 for instances variables of Context or any of its superclasses and subclasses.  The
	 convention to make the compiler use the special nodes is to use negative indices"

	| superInstSize |
	(self withAllSubclasses noneSatisfy: [:class|class isContextClass]) ifTrue:
		[^super instVarNamesAndOffsetsDo: aBinaryBlock].
	(superInstSize := superclass notNil ifTrue: [superclass instSize] ifFalse: [0]) > 0 ifTrue:
		[superclass instVarNamesAndOffsetsDo: aBinaryBlock].
	1 to: self instSize - superInstSize do:
		[:i| aBinaryBlock value: (self instVarNames at: i) value: (i + superInstSize) negated]