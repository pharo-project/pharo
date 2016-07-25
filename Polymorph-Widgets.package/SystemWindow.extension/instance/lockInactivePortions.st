*Polymorph-Widgets
lockInactivePortions
	"Make me unable to respond to mouse and keyboard.  Control boxes remain active."

	self isEmbedded ifTrue: [^self].
	self submorphsDo: [:m | m == labelArea ifFalse: [m lock]]