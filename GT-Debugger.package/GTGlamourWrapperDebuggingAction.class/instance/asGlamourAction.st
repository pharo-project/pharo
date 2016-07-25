transformation
asGlamourAction
	|actionBlock|
	actionBlock := glamourAction action.
	
	^ glamourAction
		action: [ :presentation :entity |
			self precondition ifTrue: [ 
				self preAction.
				actionBlock cull: presentation cull: entity.
				self postAction ] ]