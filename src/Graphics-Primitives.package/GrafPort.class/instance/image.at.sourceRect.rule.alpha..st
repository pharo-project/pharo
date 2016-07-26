drawing support
image: aForm at: aPoint sourceRect: sourceRect rule: rule alpha: sourceAlpha
	"Draw the portion of the given Form defined by sourceRect at the given point using the given BitBlt combination rule."

	sourceForm := aForm.
	combinationRule := rule.
	self sourceRect: sourceRect.
	self destOrigin: aPoint.
	self copyBitsTranslucent: (alpha := (sourceAlpha * 255) truncated min: 255 max: 0).