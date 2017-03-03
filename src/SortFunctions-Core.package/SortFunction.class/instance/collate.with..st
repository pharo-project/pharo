evaluating
collate: aValue with: bValue
	^ self hasUndefinedDirection
		ifTrue: [ 
			aValue isNil
				ifTrue: [ undefinedDirection * direction ]
				ifFalse: [ 
					bValue isNil
						ifTrue: [ undefinedDirection negated * direction ]
						ifFalse: [ aValue threeWayCompareTo: bValue ] ] ]
		ifFalse: [ aValue threeWayCompareTo: bValue ]