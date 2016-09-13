private
growTo: aSize 
	| newKeys newValues |
	newKeys := Array new: aSize.
	newValues := Array new: aSize.
	1 to: size
		do: 
			[:i | 
			newKeys at: i put: (keys at: i).
			newValues at: i put: (values at: i)].
	keys := newKeys.
	values := newValues