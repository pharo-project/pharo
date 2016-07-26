initialize
outOfWorld: aWorld

	Morph announcer unsubscribe: self.
	^ super outOfWorld: aWorld.