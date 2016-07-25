comparing
= other
	self class = other class ifFalse: [ ^ false ].
	^ self method = other method
		and: [ self uri asRelativeUrl = other uri asRelativeUrl
			and: [ self version = other version ] ]