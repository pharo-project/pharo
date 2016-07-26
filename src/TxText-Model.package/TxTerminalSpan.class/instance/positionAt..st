accessing-text position
positionAt: index
	index ~= 0 ifTrue:  [self error: 'invalid position' ].
	
	^ super positionAt: 0