file support
openFile: aFileName
	"Load file content into this workspace. If successful, associate the workspace with this file and change its label"
	
	self readStringFromFile: aFileName.
	self fileName: aFileName.
	self label: aFileName.