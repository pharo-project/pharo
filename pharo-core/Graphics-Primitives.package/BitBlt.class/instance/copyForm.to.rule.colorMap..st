copying
copyForm: srcForm to: destPt rule: rule colorMap: map 
	sourceForm := srcForm.
	halftoneForm := nil.
	combinationRule := rule.
	destX := destPt x + sourceForm offset x.
	destY := destPt y + sourceForm offset y.
	sourceX := 0.
	sourceY := 0.
	width := sourceForm width.
	height := sourceForm height.
	colorMap := map.
	self copyBits