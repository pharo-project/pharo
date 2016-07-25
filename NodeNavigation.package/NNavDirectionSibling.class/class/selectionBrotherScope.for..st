step over
selectionBrotherScope: selected for: node
	| brother |

	node ifNil: [ ^selected ].
	brother := self brotherNodeFor:  node.
	^brother ifNil:[nil] ifNotNil: [ brother start to: brother stop].