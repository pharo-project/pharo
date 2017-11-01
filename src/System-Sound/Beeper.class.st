"
Beeper provides simple audio (or in some other way) feedback to the user.

The recommended use is ""Beeper beep"" to give the user the equivalence of a beep. If you want to force the beep to use the primitive in the VM for beeping, then use ""Beeper beepPrimitive"". In either case, if sounds are disabled there will be no beep.

The actual beeping, when you use ""Beeper beep"", is done by sending a #play message to a registered playable object. You can register your own playable object by invoking the class side method #setDefault: passing in an object that responds to the #play message.

The default playable object is an instance of Beeper itself which implements #play on the instance side. That implementation delegates the playing of the beep to the default SoundService.

Note that #play is introduced as a common interface between AbstractSound and Beeper.
This way we can register instances of AbstractSound as playable entities, for example:

	Beeper setDefault: (SampledSound new
						setSamples: self coffeeCupClink
						samplingRate: 12000).

Then ""Beeper beep"" will play the coffeeCup sound.
"
Class {
	#name : #Beeper,
	#superclass : #Object,
	#classInstVars : [
		'default'
	],
	#category : 'System-Sound'
}

{ #category : #beeping }
Beeper class >> beep [
	"The preferred way of producing an audible feedback.
	The default playable entity (an instance of Beeper)
	also uses the pluggable SoundService
	mechanism, so it will use the primitive beep only
	if there is no other sound mechanism available."
	
	self default 
		ifNil: [self beepPrimitive]
		ifNotNil: [ self default play].

]

{ #category : #beeping }
Beeper class >> beepPrimitive [
	"Make a primitive beep. Only use this if
	you want to force this to be a primitive beep.
	Otherwise use Beeper class>>beep
	since this method bypasses the current
	registered playable entity."

	SoundSystem current soundEnabled
		ifTrue: [self primitiveBeep]
]

{ #category : #customize }
Beeper class >> clearDefault [
	"Clear the default playable.
	Will be lazily initialized in Beeper class >>default."

	default := nil
]

{ #category : #customize }
Beeper class >> default [
	"When the default is not defined it is
	initialized using #newDefault."

	^ default ifNil: [default := self newDefault ] 
]

{ #category : #customize }
Beeper class >> newDefault [
	"Subclasses may override me to provide a default beep.
	This base implementation returns an instance of Beeper
	which uses the pluggable sound service."

	^ self new
]

{ #category : #private }
Beeper class >> primitiveBeep [
	"Make a primitive beep. Not to be called directly.
	It is much better to use Beeper class>>beep
	or Beeper class>>beepPrimitive
	since this method bypasses the current
	registered playable entity and does not
	check SoundSettings class>>soundEnabled."

	<primitive: 140>
	self primitiveFailed
]

{ #category : #customize }
Beeper class >> setDefault: aPlayableEntity [
	"Set the playable entity used when making a beep.
	The playable entity should implement the message #play."

	default := aPlayableEntity
]

{ #category : #'play interface' }
Beeper >> play [
	"This is how the default Beeper makes a beep,
	by sending beep to the default sound service.
	The sound system will check if sounds are enabled."

	self class environment at: #SoundService ifPresent: [ :soundService |
		soundService default new beep ].
]
