paste buffer
objectToPaste
	"It may need to be sent #startRunning by the client"
	^ Cursor wait showWhile: [PasteBuffer veryDeepCopy]

	"PasteBuffer usableDuplicateIn: self world"
