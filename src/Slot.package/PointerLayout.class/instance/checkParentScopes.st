validation
checkParentScopes
	| parentScope superclassScope |
	parentScope := self slotScope parentScope.
	self host superclass ifNil: [ ^ self ].
	superclassScope := self host superclass classLayout slotScope.
	self 
		assert: parentScope = superclassScope
		description: 'Parent slot scope is out of sync'