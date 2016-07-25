model protocol
alwaysAccept: aBoolean
	"Set the always accept flag."
	"This flag is used when there are unsaved changes in my text field and an exterior actor tries to modify me"
	"If the flag is true, I will accept to change even if I have pending modification instead of poping up the Accept/Discard/Cancel window"


	alwaysAccept := aBoolean
