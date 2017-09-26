private
searchForObject
	
	[| objects |
	self printToDoNotice.
	objects := objectsLeft.
	objectsLeft := OrderedCollection new.
	self addBacklinkLevel.
	objects do: 
			[:each | 
			| path |
			(path := self searchVariablesIn: each) notNil 
				ifTrue: [foundBlock value: path].
			(path := self searchIndicesIn: each) notNil 
				ifTrue: [foundBlock value: path]].
	objectsLeft isEmpty] 
			whileFalse