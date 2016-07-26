displaying
drawLine: sourceForm from: beginPoint to: endPoint clippingBox: clipRect rule: anInteger fillColor: aForm 
	"Refer to the comment in 
	DisplayMedium|drawLine:from:to:clippingBox:rule:mask:." 
	
	| dotSetter |
	"set up an instance of BitBlt for display"
	dotSetter := BitBlt 
		destForm: self
		sourceForm: sourceForm
		fillColor: aForm
		combinationRule: anInteger
		destOrigin: beginPoint
		sourceOrigin: 0 @ 0
		extent: sourceForm extent
		clipRect: clipRect.
	dotSetter drawFrom: beginPoint to: endPoint