private
imagePaint: aForm at: aPoint sourceRect: sourceRect
	self canvas pathTransform
		restoreAfter: [ 
			self canvas setPaint: aForm.
			self canvas pathTransform translateBy: aPoint.
			self canvas drawShape: sourceRect]