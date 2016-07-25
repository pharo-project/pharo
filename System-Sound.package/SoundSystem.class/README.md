This is the base class for a sound system. A sound system offers a small protocol for playing sounds and making beeps. 

While SoundSystem is an abstract class, it acts as a facade to 
the actual SoundSystem.

When the SoundPackage is not loaded, SoundSystem current refers to DummySoundSystem. 

SoundSystem current gives access to the currently installed SoundSystem.

When no SoundSystem is available, a dummy one is installed as current.  
    
SoundSystem soundEnabled: true.
SoundSystem current beep


SoundSystem current: BaseSoundSystem new


