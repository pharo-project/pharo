copying
fill: destRect fillColor: grayForm rule: rule 
	"Fill with a Color, not a Form."
	sourceForm := nil.
	self fillColor: grayForm.	"sets halftoneForm"
	combinationRule := rule.
	destX := destRect left.
	destY := destRect top.
	sourceX := 0.
	sourceY := 0.
	width := destRect width.
	height := destRect height.
	self copyBits