Extension { #name : 'ZipArchive' }

{ #category : '*NewTools-Compression-Utils' }
ZipArchive class >> extractAllIn: aFileReferenceOrFileName [
	"Service method to extract all contents of a zip.
	Example: 
		ZipArchive extractAllIn: 'my_file.zip' 
		"
	| directory |

	directory := (self openDirectoryDialog
		currentDirectory: FileSystem workingDirectory;
		openModal) ifNil: [ ^ self ].

	^ self new
		readFrom: aFileReferenceOrFileName;
		extractAllTo: directory
]
