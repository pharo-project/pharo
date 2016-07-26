printing
treePrintOn: aStream tabs: tabs thisTab: myTab total: total totalTime: totalTime tallyExact: isExact orThreshold: threshold 
	| sons sonTab |
	tabs do: [:tab | aStream nextPutAll: tab].
	tabs size > 0 
		ifTrue: 
			[self 
				printOn: aStream
				total: total
				totalTime: totalTime
				tallyExact: isExact].
	sons := isExact ifTrue: [receivers] ifFalse: [self sonsOver: threshold].
	sons isEmpty 
		ifFalse: 
			[tabs addLast: myTab.
			sons := sons asSortedCollection.
			1 to: sons size do: 
					[:i | 
					sonTab := i < sons size ifTrue: ['  |'] ifFalse: ['  '].
					(sons at: i) 
						treePrintOn: aStream
						tabs: (tabs size < self maxTabs 
								ifTrue: [tabs]
								ifFalse: [(tabs select: [:x | x = '[']) copyWith: '['])
						thisTab: sonTab
						total: total
						totalTime: totalTime
						tallyExact: isExact
						orThreshold: threshold].
			tabs removeLast]