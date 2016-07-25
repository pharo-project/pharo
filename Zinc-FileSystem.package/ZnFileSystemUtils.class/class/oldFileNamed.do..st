streams
oldFileNamed: fileName do: block
	^ fileName asFileReference 
		readStreamDo: block
		ifAbsent: [ FileDoesNotExist signalWith: fileName asFileReference ]