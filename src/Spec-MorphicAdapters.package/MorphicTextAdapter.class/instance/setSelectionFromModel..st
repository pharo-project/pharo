spec protocol
setSelectionFromModel: aSelection

	self widget ifNotNil: [:w | w setSelection: aSelection ]