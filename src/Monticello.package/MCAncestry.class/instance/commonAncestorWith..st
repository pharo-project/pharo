ancestry
commonAncestorWith: aNode
	| commonAncestors |
	commonAncestors := self commonAncestorsWith: aNode.
	^ commonAncestors at: 1 ifAbsent: [nil]