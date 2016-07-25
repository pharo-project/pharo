comparing
literalEqual: other

	self class == other class ifFalse: [^ false].
	self size = other size ifFalse: [^ false].
	self with: other do: [:e1 :e2 |
		(e1 literalEqual: e2) ifFalse: [^ false]].
	^ true