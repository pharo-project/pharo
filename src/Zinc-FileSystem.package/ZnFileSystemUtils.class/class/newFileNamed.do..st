streams
newFileNamed: fileName do: block
	^ fileName asFileReference 
		writeStreamDo: block 
		ifPresent: [ FileExists signalWith: fileName asFileReference ]