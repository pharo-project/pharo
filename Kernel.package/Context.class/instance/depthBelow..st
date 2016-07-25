debugger access
depthBelow: aContext
	"Answer how many calls there are between this and aContext."

	| context depth |
	context := self.
	depth := 0.
	[ context == aContext or: [ context == nil ]]
		whileFalse: [
			context := context sender.
			depth := depth + 1 ].
	^ depth