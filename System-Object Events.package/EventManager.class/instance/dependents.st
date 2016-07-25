dependents access
dependents

	^(self actionSequenceForEvent: self changedEventSelector) asSet
		collect:
			[:each | each receiver]