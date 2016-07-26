event handling
mouseDown: evt
	"Make this TextMorph be the keyboard input focus, if it isn't  
	already, and repond to the text selection gesture.
	Changed to not take keyboard focus if an owner is a
	PluggableTextMorph that doesn't want focus."

	(evt yellowButtonPressed and: [ evt commandKeyPressed not ])
		ifTrue: [ ^ self yellowButtonActivity: evt shiftPressed ].	"First check for option (menu) click"
	(self paragraph click: evt for: self model controller: self)
		ifTrue: [ self click: evt.
			evt hand releaseKeyboardFocus: self.
			^ self ].
	(evt yellowButtonChanged or: [ evt commandKeyPressed ])
		ifTrue: [ (self yellowButtonActivity: evt shiftPressed)
				ifTrue: [ ^ self ] ].
	"no matter what, if shift is pressed extend the selection"
	evt shiftPressed
		ifTrue: [ ^ self mouseMove: evt ].
	mouseDownPoint := evt cursorPoint.
	(self hasFocus or: [ self editor hasSelection not ])
		ifTrue:
			[ | selectors |
			self scrollPivot: evt hand position.
			selectors := Array
				with: #click:
				with: #doubleClick:
				with: nil
				with: nil.
			evt hand
				waitForClicksOrDrag: self
				event: evt
				selectors: selectors
				threshold: 5 ].
	self hasKeyboardFocus
		ifFalse: [ self takeKeyboardFocus ]