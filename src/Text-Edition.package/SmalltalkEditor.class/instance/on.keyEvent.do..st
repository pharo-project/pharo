shortcuts
on: target keyEvent: event do: shortcutActionBlock
	"Setup and break-up to be able to do a shortcut."

	target textMorph
		handleInteraction: [ target textMorph editor handleEditionAction: shortcutActionBlock fromKeyboardEvent: event ].
	target textChanged.
	target scrollSelectionIntoView