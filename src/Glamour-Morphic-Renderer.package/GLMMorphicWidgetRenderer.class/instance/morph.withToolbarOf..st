private
morph: aMorph withToolbarOf: aPresentation
	| container offset dock |
	aPresentation hasActions
		ifFalse: [ ^ aMorph ].
	container := GLMMorphic containerMorph.
	offset := 0.
	dock := self renderToolbarForPresentation: aPresentation inMorph: container.
	offset := dock minExtent y.
	container
		addMorph: dock
		fullFrame:
			(LayoutFrame identity
				bottomFraction: 0; 
				bottomOffset: offset).
	container addMorph: aMorph fullFrame: (LayoutFrame identity topOffset: offset).
	^ container