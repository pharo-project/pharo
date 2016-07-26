access
messageText: aText
	"change the current text"
	messageText := aText.
	self changed: #messageText.
	^true