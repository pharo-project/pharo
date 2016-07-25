beeping
beepPrimitive
	"Make a primitive beep. Only use this if
	you want to force this to be a primitive beep.
	Otherwise use Beeper class>>beep
	since this method bypasses the current
	registered playable entity."

	SoundSystem current soundEnabled
		ifTrue: [self primitiveBeep]