private
findSourceRangeFor: aNumber 
	aNumber
		to: ranges size
		by: 2
		do: [:index | 
			index + 3 > ranges size
				ifTrue: [^ nil].
			(ranges at: index + 1) isAssignment
				ifTrue: [(ranges at: index + 3) type = #statementSeparator
						ifTrue: [^ index + 2]
						ifFalse: [(ranges at: index + 2) type = #globalVar
								ifTrue: [^ index + 2]
								ifFalse: [(ranges at: index + 3) isAssignment 
										ifFalse: [^ nil]]]]]