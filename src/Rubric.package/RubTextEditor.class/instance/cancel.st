menu messages
cancel
	"Cancel the changes made so far to this text"

	self canChangeText
		ifTrue: [ textArea cancelEdits ]