attributes
addAttribute: attr startPosition: startPosition stopPosition: stopPosition
	| start selection startIndex nodeSize |
	startIndex := startPosition - 1.
	nodeSize := stopPosition - startIndex.
	start := text startPosition + startIndex.
	selection := start selectTo: start + nodeSize.
	selection applyAttribute: attr