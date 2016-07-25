private
privateMoveBy: delta
	super privateMoveBy: delta.
	vertices := vertices collect: [:p | p + delta].
	curveState := nil.  "Force recomputation"
	(self valueOfProperty: #referencePosition) ifNotNil:
		[:oldPos | self setProperty: #referencePosition toValue: oldPos + delta]