
| localRepository files |

localRepository := MCDirectoryRepository new
	directory: './kernelPackages' asFileReference;
	yourself.
	
'protocols.txt' asFileReference writeStreamDo: [:stream |
	
files := localRepository directory files sorted.

files do: [:file |
    version := localRepository versionFrom: file basenameWithoutExtension.
    definitions := version snapshot definitions select: #isMethodDefinition.	
    definitions do: [:def |
      stream nextPutAll: def className asString; tab.
      stream nextPutAll: def classIsMeta asString; tab.
      stream nextPutAll: def selector asString; tab.
      stream nextPutAll: def category asString; cr.
]]]