*Polymorph-Widgets-override
minExtentFrom: minExtent
	"Return the minimal extent the given bounds can be represented in."
	
	| width height widthProp heightProp |
	"calculate proportional area. bottom/right offsets extend in +ve direction."
	width := minExtent x + leftOffset - rightOffset.
	height := minExtent y + topOffset - bottomOffset.
	"calculate the effective proportion"
	widthProp := rightFraction - leftFraction.
	heightProp := bottomFraction - topFraction.
	"if the proportions are 0 then the minima cannot be determined and
	minExtent cannot be respected."
	width := widthProp = 0
		ifTrue: [0]
		ifFalse: [width / widthProp].
	height := heightProp = 0
		ifTrue: [0]
		ifFalse: [height / heightProp].
	^width truncated @ height truncated