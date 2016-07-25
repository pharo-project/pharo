step over
brotherNodeFor: node
"Finds the next brother for the node"
	| brothers nodePosition nextBrotherPosition |

	node ifNil: [ ^nil ].
	node parent ifNil: [ ^nil ]. "no parent you are orphan"
	brothers := node parent children.
	nodePosition := (brothers indexOf: node) .
	nextBrotherPosition := self brotherPositionFor: nodePosition brothers: brothers .
	^ brothers at: nextBrotherPosition.
