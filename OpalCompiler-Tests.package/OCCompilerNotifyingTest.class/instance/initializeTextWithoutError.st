initialize-release
initializeTextWithoutError
	"Remove the errors from the text to be compiled and answer the text without errors.
	Meanwhile, collect the expected error messages and their expected position."

	| input output errorStream positionStream |
	input := text readStream.
	output := (String new: text size) writeStream.
	errorStream := (Array new: self numberOfSelections) writeStream.
	positionStream := (Array new: self numberOfSelections) writeStream.
	
	[output nextPutAll: (input upTo: $`).
	input atEnd]
		whileFalse:
			[positionStream nextPut: output position + 1.
			errorStream nextPut: (input upTo: $`)].
	expectedErrors := errorStream contents.
	expectedErrorPositions := positionStream contents.
	^output contents