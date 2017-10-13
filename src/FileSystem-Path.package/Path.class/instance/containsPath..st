comparing
containsPath: aPath
	self size < aPath size ifFalse: [^ false].
	1	to: self size 
		do: [:i | (self at: i) = (aPath at: i) ifFalse: [^ false]].
	^ true