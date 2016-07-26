rendering
installDefaultSelectionActions

	self defaultSelectionActions do: [ :anAction |
		self addSelectionAction: anAction ]