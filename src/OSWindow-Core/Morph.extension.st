Extension { #name : #Morph }

{ #category : #'*OSWindow-Core' }
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

{ #category : #'*OSWindow-Core' }
Morph >> openInExternalWindowModalCallback: aBlock [
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
]
