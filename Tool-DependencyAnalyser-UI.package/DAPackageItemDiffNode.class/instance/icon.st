accessing
icon
	self content isNew ifTrue: [ ^ Smalltalk ui icons iconNamed: #changeAddIcon ].
	self content isRemoved ifTrue: [^ Smalltalk ui icons iconNamed: #smallCancelIcon ].
	^ Smalltalk ui icons iconNamed: #forwardIcon