accessing - cache
valueFor: anElement withIndex: anInteger in: aGlamourColumn
	
	^ self 
		cacheAt: aGlamourColumn title 
		for: anElement 
		ifAbsentPut: [ 
			self glamourPresentation 
				column: aGlamourColumn 
				valueFor: (self glamourPresentation isWithElementIndex
					ifTrue: [ 
						anElement asGlamorousMultiValue,
						anInteger asGlamorousMultiValue ]
			ifFalse: [ anElement ]) ]


	