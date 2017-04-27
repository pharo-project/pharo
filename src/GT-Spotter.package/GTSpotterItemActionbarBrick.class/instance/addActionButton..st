adding
addActionButton: anActionButton

	self themer spotterThemer itemActionButtonStyleFor: anActionButton.
	self themer spotterThemer itemActionButtonWidgetStyleFor: anActionButton.
		
	self addBrickBack: anActionButton.

	self stepModel spotter isShiftPressed
		ifTrue: [ self themer spotterThemer hintRevealStyleFor: anActionButton ].