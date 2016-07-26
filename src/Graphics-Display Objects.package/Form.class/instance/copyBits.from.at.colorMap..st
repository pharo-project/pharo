copying
copyBits: sourceRect from: sourceForm at: destOrigin colorMap: map 
	"Make up a BitBlt table and copy the bits with the given colorMap."
	((BitBlt  
		destForm: self
		sourceForm: sourceForm
		halftoneForm: nil
		combinationRule: Form over
		destOrigin: destOrigin
		sourceOrigin: sourceRect origin
		extent: sourceRect extent
		clipRect: self boundingBox) colorMap: map) copyBits