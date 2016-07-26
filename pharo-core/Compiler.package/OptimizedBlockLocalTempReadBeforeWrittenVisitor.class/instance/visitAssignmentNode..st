visiting
visitAssignmentNode: anAssignmentNode
	anAssignmentNode value accept: self.
	anAssignmentNode variable isTemp
		ifTrue:
			[written ifNil: [written := IdentitySet new].
			 written add: anAssignmentNode variable]
		ifFalse:
			[anAssignmentNode variable accept: self]