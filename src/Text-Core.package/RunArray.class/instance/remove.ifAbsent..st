removing
remove: anObject ifAbsent: exceptionBlock
	| index mustCoalesce run |
	index := values indexOf: anObject ifAbsent: [^exceptionBlock value].
	(run := runs at: index) > 1
		ifTrue: [runs at: index put: run - 1]
		ifFalse:
			[mustCoalesce := index > 1 and: [index < values size and: [(values at: index - 1) = (values at: index + 1)]].
			runs := runs copyWithoutIndex: index.
			values := values copyWithoutIndex: index.
			mustCoalesce
				ifTrue:
					[runs at: index - 1 put: (runs at: index - 1) + (runs at: index).
					runs := runs copyWithoutIndex: index.
					values := values copyWithoutIndex: index]].
	^anObject