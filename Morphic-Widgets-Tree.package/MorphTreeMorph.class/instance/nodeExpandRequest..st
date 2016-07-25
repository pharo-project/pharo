announce requests
nodeExpandRequest: anAnnounce
	anAnnounce nodes
		ifEmpty: [self expandAll]
		ifNotEmpty: [:nodes | anAnnounce recur 
			ifTrue: [self expandAllFromNode: nodes last]
			ifFalse: [self expandNodePath: nodes]]
