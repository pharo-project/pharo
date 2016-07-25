private
imageFromName: aSymbol
	^ self images
		at: aSymbol
		ifPresent: [:block | block value]
		ifAbsent: []