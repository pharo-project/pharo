accessing
acceptOnFocusChange: aBoolean
	"Set whether the editor accepts its contents when it loses the keyboard focus."

	acceptOnFocusChange := aBoolean.
	self textMorph ifNotNil: [:t | t acceptOnFocusChange: aBoolean]