beeping
beep
	"The preferred way of producing an audible feedback.
	The default playable entity (an instance of Beeper)
	also uses the pluggable SoundService
	mechanism, so it will use the primitive beep only
	if there is no other sound mechanism available."
	
	self default 
		ifNil: [self beepPrimitive]
		ifNotNil: [ self default play].
