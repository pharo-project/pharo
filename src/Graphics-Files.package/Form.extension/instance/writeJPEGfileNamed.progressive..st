*Graphics-Files
writeJPEGfileNamed: fileName  progressive: aBoolean
	"Write a JPEG file to the given filename using default settings.  Make it progressive or not, depending on the boolean argument"

	PluginBasedJPEGReadWriter putForm: self quality: -1 "default" progressiveJPEG: aBoolean onFileNamed: fileName

"
Display writeJPEGfileNamed: 'display.jpeg' progressive: false.
Form fromUser writeJPEGfileNamed: 'yourPatch.jpeg' progressive: true
"