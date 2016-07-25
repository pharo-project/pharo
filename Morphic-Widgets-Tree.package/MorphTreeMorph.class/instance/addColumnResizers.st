column handling
addColumnResizers
	"add all needed column resizers"
	columnResizers := self columnResizerFrames
				withIndexCollect: [:frm :idx | 
					(MorphTreeResizerMorph container: self index: idx) bounds: (frm translateBy: (scroller offset x negated @ 0))].
	self addAllMorphs: columnResizers.
	self columnResizersToFront