copying
copy: destRectangle from: sourcePt in: srcForm 
	| destOrigin |
	sourceForm := srcForm.
	halftoneForm := nil.
	combinationRule := 3.	"store"
	destOrigin := destRectangle origin.
	destX := destOrigin x.
	destY := destOrigin y.
	sourceX := sourcePt x.
	sourceY := sourcePt y.
	width := destRectangle width.
	height := destRectangle height.
	self copyBits