accessing
numberOfChildren
	^ self children
		ifEmpty: [ 0 ]
		ifNotEmpty: [ :children2 | children2 size + (children2 collect: [ :item | item numberOfChildren ]) sum ]