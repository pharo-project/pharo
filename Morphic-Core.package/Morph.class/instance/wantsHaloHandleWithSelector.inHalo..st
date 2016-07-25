halos and balloon help
wantsHaloHandleWithSelector: aSelector inHalo: aHaloMorph
	"Answer whether the receiver would like to offer the halo handle with the given selector (e.g. #addCollapseHandle:)"

	(#(addDismissHandle:) includes: aSelector) ifTrue:
		[^ self resistsRemoval not].

	(#( addDragHandle: ) includes: aSelector) ifTrue:
		[^ self okayToBrownDragEasily].

	(#(addGrowHandle: addScaleHandle:) includes: aSelector) ifTrue:
		[^ self okayToResizeEasily].

	(#( addRotateHandle: ) includes: aSelector) ifTrue:
		[^ self okayToRotateEasily].

	(#(addRecolorHandle:) includes: aSelector) ifTrue:
		[^ self renderedMorph wantsRecolorHandle].

	^ true
	