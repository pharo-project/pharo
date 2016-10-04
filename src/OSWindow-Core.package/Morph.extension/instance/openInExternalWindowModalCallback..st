*OSWindow-Core
openInExternalWindowModalCallback: aBlock
	| world |
	world := OSWindowWorldMorph new extent: self extent.
	world
		layoutPolicy: ProportionalLayout new;
		clipSubmorphs: true;
		closeCallback: [ aBlock cull: self ].

	self layoutFrame: (0@0 corner: 1@1) asLayoutFrame.
	self hResizing: #spaceFill; vResizing: #spaceFill.
	self openInWorld: world.
	self onAnnouncement: MorphDeleted do: [ :announce |
		world ifNotNil: [
			world delete
		].
		world := nil.
	].
	^ world open