step into
selectionChildrenScope: selected for: node
	| children |

	node ifNil: [ ^selected ].
	children := self childNodeFor: node.
	^children ifNil: [ selected ] ifNotNil: [children start to: children stop]