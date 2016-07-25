mathematical functions
raisedToInteger: exp modulo: m
	(exp = 0) ifTrue: [^ 1].
	exp even
		ifTrue: [^ (self raisedToInteger: (exp // 2) modulo: m) squared \\ m]
		ifFalse: [^ (self * (self raisedToInteger: (exp - 1) modulo: m)) \\ m].