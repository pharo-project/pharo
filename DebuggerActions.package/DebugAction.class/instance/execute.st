actions
execute

	self precondition ifTrue: [ 
		self preAction.
		self perform: self actionSelector.
		self postAction ] 