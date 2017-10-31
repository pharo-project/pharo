temps
bindAndJuggle: name
	"This is used to insert a new temp and reorcder temps on editing.
	 It doesn't really work for closure compilation since we have multiple
	 locations for temps.  Simply signal a reparse is necessary."

	ReparseAfterSourceEditing signal