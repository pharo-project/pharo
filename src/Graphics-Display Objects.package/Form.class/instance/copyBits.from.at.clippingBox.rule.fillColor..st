copying
copyBits: sourceRect from: sourceForm at: destOrigin clippingBox: clipRect rule: rule fillColor: aForm 
	"Make up a BitBlt table and copy the bits."

	(BitBlt  
		destForm: self
		sourceForm: sourceForm
		fillColor: aForm
		combinationRule: rule
		destOrigin: destOrigin
		sourceOrigin: sourceRect origin
		extent: sourceRect extent
		clipRect: clipRect) copyBits