Beeper provides simple audio (or in some other way) feedback to the user.

The recommended use is "Beeper beep" to give the user the equivalence of a beep. If you want to force the beep to use the primitive in the VM for beeping, then use "Beeper beepPrimitive". In either case, if sounds are disabled there will be no beep.

The actual beeping, when you use "Beeper beep", is done by sending a #play message to a registered playable object. You can register your own playable object by invoking the class side method #setDefault: passing in an object that responds to the #play message.

The default playable object is an instance of Beeper itself which implements #play on the instance side. That implementation delegates the playing of the beep to the default SoundService.

Note that #play is introduced as a common interface between AbstractSound and Beeper.
This way we can register instances of AbstractSound as playable entities, for example:

	Beeper setDefault: (SampledSound new
						setSamples: self coffeeCupClink
						samplingRate: 12000).

Then "Beeper beep" will play the coffeeCup sound.