accessing
icon
	self content added
		ifTrue: [ ^ self iconNamed: #changeAddIcon ].
	self content modified
		ifTrue: [ ^ self iconNamed: #changeUpdateIcon ].
	self content removed
		ifTrue: [ ^ self iconNamed: #changeRemoveIcon ].
	^ nil