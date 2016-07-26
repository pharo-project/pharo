comparing
= other
	self class = other class ifFalse: [ ^ false ].
	^ self code = other code 
		and: [ self version = other version
			and: [ self reason = other reason ] ]