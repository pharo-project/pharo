private
newRevertEvent

	^ EpUndo forAll: (self theLog referencesToAll: self entries)
