*Iceberg-UI
icon
	self operation isAddition ifTrue: [ ^ Smalltalk ui icons iconNamed: #changeAdd ].
	self operation isModification ifTrue: [ ^ Smalltalk ui icons iconNamed: #changeUpdate ].
	self operation isRemoval ifTrue: [ ^ Smalltalk ui icons iconNamed: #changeRemove ].