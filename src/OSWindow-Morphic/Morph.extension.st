Extension { #name : 'Morph' }

{ #category : '*OSWindow-Morphic' }
Morph >> openInExternalWindow [
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
]

{ #category : '*OSWindow-Morphic' }
Morph >> openInExternalWindowModalCallback: aBlock [
	| world |
	world := OSWindowWorldMorph new extent: self extent.
	world
		layoutPolicy: ProportionalLayout new;
		clipSubmorphs: true;
		closeCallback: [ aBlock cull: self ].

	self layoutFrame: LayoutFrame identity.
	self hResizing: #spaceFill; vResizing: #spaceFill.
	self openInWorld: world.
	self onAnnouncement: MorphDeleted do: [ :announce |
		world ifNotNil: [
			world delete
		].
		world := nil.
	].
	^ world open
]
