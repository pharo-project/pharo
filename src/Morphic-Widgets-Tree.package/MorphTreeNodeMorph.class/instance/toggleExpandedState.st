updating
toggleExpandedState
	| toDelete |
	self isExpanded: self isExpanded not.
	toDelete := OrderedCollection new.
	firstChild ifNotNil: [ firstChild withSiblingsDo: [ :aNode | aNode recursiveAddTo: toDelete ] ].
	container noteRemovalOfAll: toDelete.
	(isExpanded and: [ complexContents hasContents ])
		ifFalse: [ ^ self changed ].
	self expand