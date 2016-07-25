accessing
firstNodeMorph
	"returns the first scroller submorph if not empty, or nil if empty"
	^ self allNodeMorphs ifEmpty: [nil] ifNotEmpty: [scroller submorphs at: 1]