event handling
mouseDown: evt
	"Handle the transitions."
	
	self perform: #mouseDown: withArguments: {evt} inSuperclass: Morph.
	self enabled ifFalse: [^self].
	self isOn
		ifTrue: [self state: #repressed]
		ifFalse: [self state: #pressed].
	actWhen == #buttonDown
		ifTrue:
			[self doButtonAction].
	self mouseStillDown: evt.