copying
copyForm: srcForm to: destPt rule: rule color: color 
	sourceForm := srcForm.
	halftoneForm := color.
	combinationRule := rule.
	destX := destPt x + sourceForm offset x.
	destY := destPt y + sourceForm offset y.
	sourceX := 0.
	sourceY := 0.
	width := sourceForm width.
	height := sourceForm height.
	self copyBits