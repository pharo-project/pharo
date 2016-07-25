printing/formatting
excerpt
	"Return a short excerpt of the text of the message"

	^ self bodyText withSeparatorsCompacted truncateWithElipsisTo: 60