announce requests
nodeCollapseRequest: anAnnounce
	anAnnounce nodes
		ifEmpty: [self collapseAll]
		ifNotEmpty: [:nodes | self collapseNodePath: nodes]
