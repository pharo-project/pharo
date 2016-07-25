traversing
markDirtyChildrenAround: aBrick inContext: aContextBrick

	aBrick subbricks do: [ :each |
		each = aContextBrick ifFalse: [
			each isBrick ifTrue: [
			(self isClean: each) ifTrue: [
				self markDirty: each.
				self markDirtyAround: each inContext: aBrick ] ] ] ].