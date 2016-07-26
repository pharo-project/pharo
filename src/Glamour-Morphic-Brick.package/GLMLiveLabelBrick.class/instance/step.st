stepping and presenter
step
	|newText|
	newText := self textLogic value asString.
	self text = newText ifTrue:  [ ^ self ].
	self text: newText.
	self doLayoutForce