miscellaneous
logStdErrorDuring: aBlock
	| stderr |
	[
		"install the line end conversion and force initialize the converter"
		stderr := ZnCrPortableWriteStream on: (ZnCharacterWriteStream
			on: Stdio stderr
			encoding: 'utf8').
		
		"log in red"
		stderr nextPut: Character escape; nextPutAll: '[31m'.
		"rund the loggin block"
		aBlock value: stderr.
		"reset the coloring"
		stderr nextPut: Character escape; nextPutAll: '[0m'.
	] on: Error do: [ :e| "we don't care if the logging to stdout fails..." ].