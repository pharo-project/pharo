event handling
mouseMove: evt
	"Check for straying."
	
	self perform: #mouseMove: withArguments: {evt} inSuperclass: Morph.
	self enabled ifFalse: [^self].
	(self containsPoint: evt cursorPoint)
		ifTrue: [state == #on
					ifTrue: [self state: #repressed].
				state == #off
					ifTrue: [self state: #pressed]]
		ifFalse: [state == #repressed
					ifTrue: [self state: #on].
				state == #pressed
					ifTrue: [self state: #off]]