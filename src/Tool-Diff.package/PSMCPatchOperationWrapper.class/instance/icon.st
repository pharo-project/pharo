accessing
icon
	"Answer a form with an icon to represent the receiver"

	self item localChosen
		ifTrue: [ ^ self iconNamed: #changeBlockIcon ].
	^ super icon