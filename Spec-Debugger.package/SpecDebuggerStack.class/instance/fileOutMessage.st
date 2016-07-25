actions browsing
fileOutMessage
	"Put a description of the selected message on a file"

	self currentMessageName ifNotNil: [
		Cursor write showWhile:
			[ self currentClassOrMetaClass fileOutMethod: self currentMessageName ] ]