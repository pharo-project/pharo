private
newApplyEvent

	^ EpRedo forAll: (self theLog referencesToAll: self entries)
