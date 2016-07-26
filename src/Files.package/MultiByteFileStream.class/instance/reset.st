open/close
reset

	super reset.
	converter ifNil: [
		self converter: UTF8TextConverter new.
	].
