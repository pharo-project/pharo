actions
narrowCompletion
	| model |
	
	self selected: 0.
	firstVisible := 1.
	model := context model.
	model narrowWith: context completionToken.
	model notEmpty ifTrue: [self selected: 1].
	model entries size < self selected ifTrue: [self selected: model entries size].
	self show.
	^ true