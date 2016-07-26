comparing
= other

	self species == other species ifFalse: [^ false].
	1 to: self class instSize do:
		[:i | (self instVarAt: i) == (other instVarAt: i) ifFalse: [^ false]].
	^ true