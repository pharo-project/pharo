private
newLinesBeforeStartingAt: anIndex
	| count cr lf index char |
	originalSource ifNil: [ ^ 0 ].
	(anIndex isNil or: [ anIndex > originalSource size ])
		ifTrue: [ ^ 0 ].
	cr := Character value: 13.
	lf := Character value: 10.
	count := 0.
	index := anIndex - 1.
	[ index > 0 and: [ (char := originalSource at: index) isSeparator ] ]
		whileTrue: [ 
			char == lf
				ifTrue: [ 
					count := count + 1.
					(originalSource at: (index - 1 max: 1)) == cr
						ifTrue: [ index := index - 1 ] ].
			char == cr
				ifTrue: [ count := count + 1 ].
			index := index - 1 ].
	^ count