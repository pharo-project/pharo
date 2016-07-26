accessing
textArea: aClient
	"Classically, the client is a Morph which holds the text, the textstyle, the textColor.
	client must never be nil except when I'm released"
	textArea := aClient 