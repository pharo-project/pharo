copying
copy: destRectangle from: sourcePt in: srcForm fillColor: hf rule: rule 
	"Specify a Color to fill, not a Form."
	| destOrigin |
	sourceForm := srcForm.
	self fillColor: hf.	"sets halftoneForm"
	combinationRule := rule.
	destOrigin := destRectangle origin.
	destX := destOrigin x.
	destY := destOrigin y.
	sourceX := sourcePt x.
	sourceY := sourcePt y.
	width := destRectangle width.
	height := destRectangle height.
	srcForm == nil ifFalse: [ colorMap := srcForm colormapIfNeededFor: destForm ].
	^ self copyBits