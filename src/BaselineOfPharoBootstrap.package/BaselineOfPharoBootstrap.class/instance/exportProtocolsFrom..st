utilities
exportProtocolsFrom: aDirectoryRepository

	"Export file named protocols.txt that describes protocol name
	for all methods defined by the packages in the specified 
	repository aRepository. This list is required to correctly
	initialize RPackage organization in the bootstrapped image.
	
	| repository |
	repository := MCDirectoryRepository new
		directory: './kernelPackages' asFileReference;
		yourself.
	BaselineOfPharoBootstrap new exportProtocolsFrom: repository.
	"
		
	| files |

	'protocols.txt' asFileReference writeStreamDo: [:stream |
		
	files := aDirectoryRepository directory files sorted.

	files do: [:file |
		| version definitions |
		version := aDirectoryRepository versionFrom: file basenameWithoutExtension.
		definitions := version snapshot definitions select: #isMethodDefinition.	
		definitions do: [:def |
			stream nextPutAll: def className asString; tab.
			stream nextPutAll: def classIsMeta asString; tab.
			stream nextPutAll: def selector asString; tab.
			stream nextPutAll: def category asString; cr. 	]]].