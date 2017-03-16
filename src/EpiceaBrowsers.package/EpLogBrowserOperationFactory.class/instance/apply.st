public
apply

	self
		trigger: [ self entries do: [:each |
			self handleErrorDuring: [ each content accept: EpApplyVisitor new ]]] 
		with: self applyEvent