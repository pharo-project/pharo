play interface
play
	"This is how the default Beeper makes a beep,
	by sending beep to the default sound service.
	The sound system will check if sounds are enabled."

	(#SoundService asClassIfAbsent: [ ^ self ]) default new beep