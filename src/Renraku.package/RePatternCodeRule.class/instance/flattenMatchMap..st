private
flattenMatchMap: aDictionary
	^ aDictionary keysAndValuesDo: [ :key :val |
		((key isKindOf: RBProgramNode) and: [ 
		 key isPatternNode ])
			ifTrue: [
				aDictionary
					at: key pattern
					put: val ] ] 