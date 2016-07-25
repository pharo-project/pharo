accessing
temporaries
	| tempRanges |
	tempRanges := ranges select: [ :each | #(#patternTempVar #patternArg ) includes: each type ].
	tempRanges addAll: self blockTemporaries.
	^ tempRanges collect: 
		[ :each | 
		NECLocalEntry 
			contents: (self sourceOf: each)
			type: each type ]