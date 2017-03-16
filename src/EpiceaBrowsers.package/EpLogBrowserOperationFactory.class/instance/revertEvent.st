private
revertEvent

	^ EpUndo forAll: (self theLog referencesToAll: self entries)
