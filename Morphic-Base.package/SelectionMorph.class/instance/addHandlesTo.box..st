halos and balloon help
addHandlesTo: aHaloMorph box: box
	| onlyThese |
	aHaloMorph haloBox: box.
	onlyThese := #(addDismissHandle: addMenuHandle: addGrabHandle: addDragHandle: addDupHandle: addHelpHandle: addGrowHandle: addFontSizeHandle: addFontStyleHandle: addFontEmphHandle: addRecolorHandle:).
	HaloMorph currentHaloSpecifications do:
		[:aSpec | (onlyThese includes: aSpec addHandleSelector) ifTrue:
				[aHaloMorph perform: aSpec addHandleSelector with: aSpec]].
	aHaloMorph innerTarget addOptionalHandlesTo: aHaloMorph box: box