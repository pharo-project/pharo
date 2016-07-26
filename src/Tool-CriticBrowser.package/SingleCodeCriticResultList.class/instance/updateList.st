thread
updateList

	self criticsOf: rule.
	thread ifNotNil: [ thread terminate ].
	thread := [ UIManager default defer:  [ criticsModel updateList ]] fork.