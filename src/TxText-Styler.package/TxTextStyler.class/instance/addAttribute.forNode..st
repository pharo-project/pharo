attributes
addAttribute: attr forNode: node
	| start selection startIndex nodeSize |
	startIndex := node start - 1.
	nodeSize := node stop - startIndex.
	start := text startPosition + startIndex.
	selection := start selectTo: start + nodeSize.
	selection applyAttribute: attr.