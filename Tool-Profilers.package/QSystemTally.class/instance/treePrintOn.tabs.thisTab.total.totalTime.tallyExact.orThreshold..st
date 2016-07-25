report
treePrintOn: aStream tabs: tabs thisTab: myTab total: total totalTime: totalTime tallyExact: isExact orThreshold: threshold
	"aStream contains the output of the print "

	"tabs is a collection of strings "

	| sons sonTab |
	tabs do: [ :tab | aStream nextPutAll: tab ].
	tabs size > 0
		ifTrue: [ 
			self
				printOn: aStream
				total: total
				totalTime: totalTime
				tallyExact: isExact ].
	sons := isExact
		ifTrue: [ receivers ]
		ifFalse: [ self sonsOver: threshold ].
	sons isNil
		ifTrue: [ ^ self ].
	sons notEmpty
		ifTrue: [ 
			tabs addLast: myTab.
			sons := self asSortedCollection: sons.
			1 to: sons size do: [ :i | 
				sonTab := i < sons size
					ifTrue: [ '  |' ]
					ifFalse: [ '  ' ].
				(sons at: i)
					treePrintOn: aStream
					tabs: (self getNewTabsFor: tabs)
					thisTab: sonTab
					total: total
					totalTime: totalTime
					tallyExact: isExact
					orThreshold: threshold ].
			tabs removeLast ]