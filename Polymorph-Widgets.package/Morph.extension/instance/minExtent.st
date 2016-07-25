*Polymorph-Widgets
minExtent
	"Layout specific. Return the minimum size the receiver can be represented in.
	Implementation note: When this message is sent from an owner trying to lay out its children it will traverse down the morph tree and recompute the minimal arrangement of the morphs based on which the minimal extent is returned. When a morph with some layout strategy is encountered, the morph will ask its strategy to compute the new arrangement. However, since the final size given to the receiver is unknown at the point of the query, the assumption is made that the current bounds of the receiver are the base on which the layout should be computed. This scheme prevents strange layout changes when for instance, a table is contained in another table. Unless the inner table has been resized manually (which means its bounds are already enlarged) the arrangement of the inner table will not change here. Thus the entire layout computation is basically an iterative process which may have different results depending on the incremental changes applied.
	Fixed for shrinkWrap."

	| layout minExtent extra hFit vFit |
	hFit := self hResizing.
	vFit := self vResizing.
	(hFit == #rigid and: [vFit == #rigid]) 
		ifTrue: 
			["The receiver will not adjust to parents layout by growing or shrinking,
		which means that an accurate layout defines the minimum size."

			^self fullBounds extent max: self minWidth @ self minHeight].

	"An exception -- a receiver with #shrinkWrap constraints but no children is being treated #rigid (the equivalent to a #spaceFill receiver in a non-layouting owner)"
	self hasSubmorphs 
		ifFalse: 
			[hFit == #shrinkWrap ifTrue: [hFit := #rigid].
			vFit == #shrinkWrap ifTrue: [vFit := #rigid]].
	layout := self layoutPolicy.
	layout 
		ifNil: [minExtent := 0 @ 0]
		ifNotNil: [minExtent := layout minExtentOf: self in: self layoutBounds].
	hFit == #rigid 
		ifTrue: [minExtent := self fullBounds extent x @ minExtent y]
		ifFalse: 
			[extra := self bounds width - self layoutBounds width.
			minExtent := (minExtent x + extra) @ minExtent y].
	minExtent := vFit == #rigid 
				ifTrue: [minExtent x @ self fullBounds extent y]
				ifFalse: 
					[extra := self bounds height - self layoutBounds height.
					minExtent x @ (minExtent y + extra)].
	minExtent := minExtent max: self minWidth @ self minHeight.
	^minExtent