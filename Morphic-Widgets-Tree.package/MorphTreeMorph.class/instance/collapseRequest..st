announce requests
collapseRequest: anAnnounce
	anAnnounce nodes
		ifEmpty: [^ self collapseAll]
		ifNotEmpty: [:nodes | self collapseNodePath: nodes]

