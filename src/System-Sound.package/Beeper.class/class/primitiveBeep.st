private
primitiveBeep
	"Make a primitive beep. Not to be called directly.
	It is much better to use Beeper class>>beep
	or Beeper class>>beepPrimitive
	since this method bypasses the current
	registered playable entity and does not
	check SoundSettings class>>soundEnabled."

	<primitive: 140>
	self primitiveFailed