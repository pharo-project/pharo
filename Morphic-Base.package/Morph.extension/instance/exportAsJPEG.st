*Morphic-Base
exportAsJPEG
	"Export the receiver's image as a JPEG"
	
	self exportAs: 'jpeg' using: PluginBasedJPEGReadWriter
