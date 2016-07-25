utilities
indexForInserting: aPoint inList: morphList horizontal: aBool target: aMorph 
	| cmp1 cmp2 cmp3 noWrap |
	properties := aMorph layoutProperties.
	noWrap := properties wrapDirection == #none.
	aBool 
		ifTrue: 
			["horizontal"

			properties listDirection == #rightToLeft 
				ifTrue: [cmp1 := [:rect | aPoint x > rect left]]
				ifFalse: [cmp1 := [:rect | aPoint x < rect right]].
			properties wrapDirection == #bottomToTop 
				ifTrue: 
					[cmp2 := [:rect | aPoint y > rect top].
					cmp3 := [:rect | aPoint y > rect bottom]]
				ifFalse: 
					[cmp2 := [:rect | aPoint y < rect bottom].
					cmp3 := [:rect | aPoint y < rect top]]]
		ifFalse: 
			["vertical"

			properties listDirection == #bottomToTop 
				ifTrue: [cmp1 := [:rect | aPoint y > rect top]]
				ifFalse: [cmp1 := [:rect | aPoint y < rect bottom]].
			properties wrapDirection == #rightToLeft 
				ifTrue: 
					[cmp2 := [:rect | aPoint x > rect left].
					cmp3 := [:rect | aPoint x > rect right]]
				ifFalse: 
					[cmp2 := [:rect | aPoint x < rect right].
					cmp3 := [:rect | aPoint x < rect left]]]. 
	morphList keysAndValuesDo: 
			[:index :m | | box | 
			self flag: #arNote.	"it is not quite clear if we can really use #fullBounds here..."
			box := m fullBounds.
			noWrap 
				ifTrue: 
					["Only in one direction"

					(cmp1 value: box) ifTrue: [^index]]
				ifFalse: 
					["Check for inserting before current row"

					(cmp3 value: box) ifTrue: [^index].
					"Check for inserting before current cell"
					((cmp1 value: box) and: [cmp2 value: box]) ifTrue: [^index]]].
	^morphList size + 1