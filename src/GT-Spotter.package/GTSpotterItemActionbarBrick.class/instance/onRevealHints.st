event handling
onRevealHints

	self visible ifFalse: [ ^ self ].
	self themer spotterThemer hintRevealStyleFor: self diveInButton.
	self themer spotterThemer hintRevealStyleFor: self actOnButton