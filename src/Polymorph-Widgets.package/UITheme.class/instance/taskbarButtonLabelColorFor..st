basic-colors
taskbarButtonLabelColorFor: aButton
	"Answer the colour for the label of the given taskbar button."

	^aButton model
		ifNil: [Color black]
		ifNotNil: [:win |
			win isActive
				ifTrue: [self selectionColor darker]
				ifFalse: [Color black]]