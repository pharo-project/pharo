building
specWindowFrom: aSpec
	| windowModel specWindow |
	windowModel := aSpec window.
	specWindow := windowModel window.
	^ specWindow