*Graphics-Files
writeJPEGfileNamed: fileName 
	"Write a JPEG file to the given filename using default settings"

	self writeJPEGfileNamed: fileName progressive: false

"
Display writeJPEGfileNamed: 'display.jpeg'
Form fromUser writeJPEGfileNamed: 'yourPatch.jpeg'
"