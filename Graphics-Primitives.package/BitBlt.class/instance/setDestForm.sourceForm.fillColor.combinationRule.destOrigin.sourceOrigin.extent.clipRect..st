private
setDestForm: df sourceForm: sf fillColor: hf combinationRule: cr destOrigin: destOrigin sourceOrigin: sourceOrigin extent: extent clipRect: clipRect 
	| aPoint |
	destForm := df.
	sourceForm := sf.
	self fillColor: hf.	"sets halftoneForm"
	combinationRule := cr.
	destX := destOrigin x.
	destY := destOrigin y.
	sourceX := sourceOrigin x.
	sourceY := sourceOrigin y.
	width := extent x.
	height := extent y.
	aPoint := clipRect origin.
	clipX := aPoint x.
	clipY := aPoint y.
	aPoint := clipRect corner.
	clipWidth := aPoint x - clipX.
	clipHeight := aPoint y - clipY.
	sourceForm == nil ifFalse: [ colorMap := sourceForm colormapIfNeededFor: destForm ]