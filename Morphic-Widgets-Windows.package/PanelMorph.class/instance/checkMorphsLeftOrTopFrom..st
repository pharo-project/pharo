protocol
checkMorphsLeftOrTopFrom: splitter
	| index size |
	
	size := submorphs size.
	index := submorphs identityIndexOf: splitter.
	
	index := index + 1.
	
	[ index <= size ] whileTrue: [ 
		| submorph done |
		submorph := submorphs at: index.
		splitter addLeftOrTop: submorph.
		done := self isResizeableMorph: submorph forSplitter: splitter.
		done
			ifTrue: [ index := size + 1 ].
		index := index + 1 ]