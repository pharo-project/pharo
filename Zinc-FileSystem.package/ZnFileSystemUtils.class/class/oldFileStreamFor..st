streams
oldFileStreamFor: fileName
	^ fileName asFileReference 
		readStreamIfAbsent: [ FileDoesNotExist signalWith: fileName asFileReference ]