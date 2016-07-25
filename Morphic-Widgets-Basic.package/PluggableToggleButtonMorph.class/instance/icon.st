accessing
icon
	| originalIcon |
	originalIcon := super icon.
	originalIcon ifNil: [ ^ originalIcon ].
	^ self isPressed 
		ifTrue: [ (originalIcon asFormOfDepth: 32) dimmed ]
		ifFalse: [ originalIcon ]