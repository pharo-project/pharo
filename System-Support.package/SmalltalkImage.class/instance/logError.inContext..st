miscellaneous
logError: errMsg inContext: aContext

	" we should think about integrating a toothpick here someday"
	self logStdErrorDuring: [ :stderr|
		"install the line end conversion and force initialize the converter"
		stderr
			nextPutAll: errMsg; cr;
			"reset the color"
			nextPut: Character escape; nextPutAll: '[0m'.
		aContext shortDebugStackOn: stderr.].
				
	self logDuring: [:logger | 
		logger	
			nextPutAll: 'THERE_BE_DRAGONS_HERE'; cr; 
		  	nextPutAll: errMsg; cr.
			"just print the error message if no context is given"
			aContext ifNotNil: [
				aContext errorReportOn: logger ].
		" write some type of separator"
		logger nextPutAll: (String new: 79 withAll: $- ); cr; cr]