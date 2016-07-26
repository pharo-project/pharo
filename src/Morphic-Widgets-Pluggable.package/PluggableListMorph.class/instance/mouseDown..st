event handling
mouseDown: evt
	"Changed to only take focus if wanted."

	| selectors row |
	row := self rowAtLocation: evt position.
	evt yellowButtonPressed
		ifTrue: [ 
			self isMultipleSelection
				ifTrue: [ 
					evt commandKeyPressed
						ifFalse: [ 
							"right click"
							(self yellowButtonActivity: evt shiftPressed)
								ifTrue: [ ^ super mouseDown: evt ] ] ]
				ifFalse: [ 
					(self yellowButtonActivity: evt shiftPressed)
						ifTrue: [ ^ super mouseDown: evt ] ] ].	"First check for option (menu) click"
	self enabled
		ifFalse: [ ^ super mouseDown: evt ].
	self wantsKeyboardFocus
		ifTrue: [ self takeKeyboardFocus ].
	row := self rowAtLocation: evt position.
	row = 0
		ifTrue: [ ^ super mouseDown: evt ].
	self mouseDownRow: row.
	self isMultipleSelection
		ifTrue: [ self mouseDownOnMultiple: evt forRow: row ].
	selectors := Array
		with: #click:
		with: (doubleClickSelector ifNotNil: [ #doubleClick: ])
		with: nil
		with:
			(self dragEnabled
				ifTrue: [ #startDrag: ]
				ifFalse: [ nil ]).
	evt hand
		waitForClicksOrDrag: self
		event: evt
		selectors: selectors
		threshold: 10.	"pixels"
	super mouseDown: evt