evaluating
collateProperty: aValue1 with: aValue2
	^ self hasUndefinedDirection
		ifTrue: [ 
			aValue1 isNil
				ifTrue: [ undefinedDirection * direction ]
				ifFalse: [ 
					aValue2 isNil
						ifTrue: [ undefinedDirection negated * direction ]
						ifFalse: [ aValue1 threeWayCompareTo: aValue2 ] ] ]
		ifFalse: [ aValue1 threeWayCompareTo: aValue2 ]