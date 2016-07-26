private
setEmphasisHere
	self editingState
		emphasisHere:
			((self text attributesAt: (self pointIndex - 1 max: 1) forStyle: self textStyle) select: [ :att | att mayBeExtended ])