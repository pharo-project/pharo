actions
narrowCompletion
	| model |
	self selected: 0.
	firstVisible := 1.
	model := context model.
	model narrowWith: context completionToken.
	(model entries size = 1 and: [ model entries first contents = context completionToken ]) ifTrue: [
		self delete.
		^ false ].
	model notEmpty ifTrue: [ self selected: 1 ].
	self show.
	^ true