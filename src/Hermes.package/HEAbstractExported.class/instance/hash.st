comparing
hash
	^ self valuesToCompareForEquality
		inject: self species hash
		into: [ :hash :comparisonValue | (hash bitXor: comparisonValue hash) hashMultiply ]