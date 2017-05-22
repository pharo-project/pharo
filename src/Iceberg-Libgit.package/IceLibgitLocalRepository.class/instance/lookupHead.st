private
lookupHead
	self withRepoDo: [ :repo | 
		| head |
		repo isUnborn ifTrue: [ ^ IceUnbornBranch new ].
		head := repo head.
		^ head isBranch 
			ifTrue: [ IceLocalBranch named: head basename inRepository: self frontend ] 
			ifFalse: [ head object asIcebergObjectInRepository: self ] ]