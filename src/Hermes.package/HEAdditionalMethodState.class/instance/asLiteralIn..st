converting
asLiteralIn: env
	| additionalMethodState |
	additionalMethodState := AdditionalMethodState new: value value size.
	additionalMethodState method: env newMethod.
	additionalMethodState selector: (selector asLiteralIn: env).

	value value withIndexDo: [ :e :idx | additionalMethodState basicAt: idx put: (e asLiteralIn: env) ].
	^ additionalMethodState