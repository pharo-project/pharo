*OSWindow-Core
openInExternalWindow
	| world |
	world := OSWindowWorldMorph new extent: self fullBounds extent.
	world
		layoutPolicy: ProportionalLayout new;
		clipSubmorphs: true.

	self hResizing: #spaceFill; vResizing: #spaceFill.
	self openInWorld: world.
	self onAnnouncement: MorphDeleted do: [ :announce |
		world ifNotNil: [
			world delete
		].
		world := nil.
	].
	^ world open