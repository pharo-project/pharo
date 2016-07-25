accessing
icon
	self content added
		ifTrue: [ ^ Smalltalk ui icons iconNamed: #changeAddIcon ].
	self content modified
		ifTrue: [ ^ Smalltalk ui icons iconNamed: #changeUpdateIcon ].
	self content removed
		ifTrue: [ ^ Smalltalk ui icons iconNamed: #changeRemoveIcon ].
	^ nil