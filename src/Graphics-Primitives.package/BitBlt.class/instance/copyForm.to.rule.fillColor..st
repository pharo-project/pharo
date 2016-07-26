copying
copyForm: srcForm to: destPt rule: rule fillColor: color 
	sourceForm := srcForm.
	self fillColor: color.	"sets halftoneForm"
	combinationRule := rule.
	destX := destPt x + sourceForm offset x.
	destY := destPt y + sourceForm offset y.
	sourceX := 0.
	sourceY := 0.
	width := sourceForm width.
	height := sourceForm height.
	self copyBits