customize
newDefault
	"Subclasses may override me to provide a default beep.
	This base implementation returns an instance of Beeper
	which uses the pluggable sound service."

	^ self new