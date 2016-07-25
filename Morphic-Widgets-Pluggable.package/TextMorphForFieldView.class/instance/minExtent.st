accessing
minExtent
	"Overridden to allow text to shrink to minimum extent rather than being (incorrectly)
	treated as rigid. Should be the same for other morphs too. Bah!"

	| layout minExtent extra hFit vFit |
	hFit := self hResizing.
	vFit := self vResizing.
	(hFit == #spaceFill or: [vFit == #spaceFill]) 
		ifFalse: 
			["The receiver will not adjust to parents layout by growing or shrinking,
		which means that an accurate layout defines the minimum size."

			^self fullBounds extent].
	layout := self layoutPolicy.
	minExtent := layout isNil 
		ifTrue: [0 @ 0]
		ifFalse: [layout minExtentOf: self in: self layoutBounds].
	minExtent := hFit == #rigid 
		ifTrue: [self fullBounds extent x @ minExtent y]
		ifFalse: 
			[extra := self bounds width - self layoutBounds width.
			(minExtent x + extra) @ minExtent y].
	minExtent := vFit == #rigid 
				ifTrue: [minExtent x @ self fullBounds extent y]
				ifFalse: 
					[extra := self bounds height - self layoutBounds height.
					minExtent x @ (minExtent y + extra)].
	minExtent := minExtent max: self minWidth @ self minHeight.
	^minExtent