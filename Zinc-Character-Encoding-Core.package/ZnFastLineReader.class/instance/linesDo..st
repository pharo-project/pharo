accessing
linesDo: block
	[ self atEnd ]
		whileFalse: [ 
			| line |
			line := self nextLine.
			line isNil
				ifFalse: [ block value: line ] ]