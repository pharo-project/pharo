accessing
icon
	self content isNew ifTrue: [ ^ self iconNamed: #changeAddIcon ].
	self content isRemoved ifTrue: [^ self iconNamed: #smallCancelIcon ].
	^ self iconNamed: #forwardIcon