private
applyEvent

	^ EpRedo forAll: (self theLog referencesToAll: self entries)
