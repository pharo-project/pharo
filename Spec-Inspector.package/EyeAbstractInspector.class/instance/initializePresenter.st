initialization
initializePresenter
	object whenChangedDo: [ self objectChanged ].
	self text
		whenBuiltDo: [ :w | w widget editingMode classOrMetaClass: self object class ].
	self initializeShortcuts