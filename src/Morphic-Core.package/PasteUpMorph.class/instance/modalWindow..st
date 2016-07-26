accessing
modalWindow: aMorph
	(self valueOfProperty: #modalWindow) ifNotNil: [ :morph | morph delete ].
	self setProperty: #modalWindow toValue: aMorph.
	aMorph ifNotNil: [ self when: #aboutToLeaveWorld send: #removeModalWindow to: self ]