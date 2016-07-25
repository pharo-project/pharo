accessing
localFileName
	"Answer my fileName in terms of the local directory naming convention"
	^ fileName copyReplaceAll: '/' with: FileSystem disk delimiter asString.