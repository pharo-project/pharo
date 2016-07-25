protocol
checkMorphsRightOrBottomFrom: splitter
	| index |
	
	index := submorphs identityIndexOf: splitter.
	index := index - 1.
	
	[ index > 0 ] whileTrue: [ | submorph done |
		submorph := submorphs at: index.
		splitter addRightOrBottom: submorph.
		done := self isResizeableMorph: submorph forSplitter: splitter.
		done ifTrue: [ index := 0 ].
		index := index - 1 ]