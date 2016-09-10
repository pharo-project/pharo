accessing
icon
	"Answer a form with an icon to represent the receiver"

	self isConflict
		ifTrue: [ self localChosen
				ifTrue: [ ^ self iconNamed: #changeBlockIcon ] ].
	^ super icon