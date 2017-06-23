comparing
= anotherBranch
	self species = anotherBranch species ifFalse: [ ^ false ].
	self repository = anotherBranch repository ifFalse: [ ^ false ].
	^ self name = anotherBranch name