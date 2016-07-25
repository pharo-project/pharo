private
morph: aMorph withToolbarOf: aPresentation
	| container offset dock |
	aPresentation hasActions ifFalse: [ ^ aMorph ].
	container := GLMMorphic containerMorph.
	offset := 0.
	dock := self renderToolbarForPresentation: aPresentation inMorph: container.
	offset := dock minExtent y.
	container addMorph: dock fullFrame: (LayoutFrame fractions: (0 @ 0 corner: 1 @ 0) offsets: (0 @ 0 corner: 0 @ offset)).	
	container 
		addMorph: aMorph 
		fullFrame: (LayoutFrame fractions: (0 @ 0 corner: 1 @ 1) offsets: (0 @ offset corner: 0 @ 0)).
	^ container