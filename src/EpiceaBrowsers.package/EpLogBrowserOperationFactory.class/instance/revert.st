public
revert

	self
		trigger: [ self entries do: [:each |
			self handleErrorDuring: [ each content accept: EpRevertVisitor new ]]]
		with: self revertEvent
