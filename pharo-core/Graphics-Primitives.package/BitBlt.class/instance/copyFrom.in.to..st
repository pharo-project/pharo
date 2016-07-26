copying
copyFrom: sourceRectangle in: srcForm to: destPt 
	| sourceOrigin |
	sourceForm := srcForm.
	halftoneForm := nil.
	combinationRule := 3.	"store"
	destX := destPt x.
	destY := destPt y.
	sourceOrigin := sourceRectangle origin.
	sourceX := sourceOrigin x.
	sourceY := sourceOrigin y.
	width := sourceRectangle width.
	height := sourceRectangle height.
	colorMap := srcForm colormapIfNeededFor: destForm.
	self copyBits