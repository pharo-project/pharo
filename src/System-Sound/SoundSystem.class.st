"
This is the base class for a sound system. A sound system offers a small protocol for playing sounds and making beeps. 

While SoundSystem is an abstract class, it acts as a facade to 
the actual SoundSystem.

When the SoundPackage is not loaded, SoundSystem current refers to DummySoundSystem. 

SoundSystem current gives access to the currently installed SoundSystem.

When no SoundSystem is available, a dummy one is installed as current.  
    
SoundSystem soundEnabled: true.
SoundSystem current beep


SoundSystem current: BaseSoundSystem new



"
Class {
	#name : #SoundSystem,
	#superclass : #Object,
	#classVars : [
		'Current',
		'SoundEnabled',
		'SoundQuickStart'
	],
	#category : #System-Sound
}

{ #category : #'current ' }
SoundSystem class >> current [

	^ Current ifNil: [ Current := DummySoundSystem new ]
]

{ #category : #'current ' }
SoundSystem class >> current: aSoundSystem [

	Current ifNotNil: [Current deactivate].
	Current := aSoundSystem.
	Current activate.
]

{ #category : #'current ' }
SoundSystem class >> resetSoundSystem [
	Current := nil
]

{ #category : #setting }
SoundSystem class >> soundEnabled [
	^ SoundEnabled ifNil: [SoundEnabled := false]
]

{ #category : #setting }
SoundSystem class >> soundEnabled: aBoolean [
	SoundEnabled := aBoolean
]

{ #category : #setting }
SoundSystem class >> soundQuickStart [
	^ SoundQuickStart ifNil: [SoundQuickStart := false]
]

{ #category : #setting }
SoundSystem class >> soundQuickStart: aBoolean [
	SoundQuickStart := aBoolean
]

{ #category : #'activation hooks' }
SoundSystem >> activate [
	"Hook for possible specialization when activating a sound system"
]

{ #category : #beep }
SoundSystem >> beep [
	"Make a primitive beep."

	self subclassResponsibility
]

{ #category : #'activation hooks' }
SoundSystem >> deactivate [
	"Hook for possible specialization when activating a sound system"
]

{ #category : #sounds }
SoundSystem >> playSampledSound: samples rate: rate [

	self subclassResponsibility
]

{ #category : #sounds }
SoundSystem >> playSoundNamed: soundName [

	self subclassResponsibility
]

{ #category : #sounds }
SoundSystem >> playSoundNamed: soundName ifAbsentReadFrom: aifFileName [

	self subclassResponsibility
]

{ #category : #sounds }
SoundSystem >> playSoundNamedOrBeep: soundName [

	self subclassResponsibility
]

{ #category : #'to deprecate' }
SoundSystem >> randomBitsFromSoundInput: bitCount [

	self subclassResponsibility
]

{ #category : #'to deprecate' }
SoundSystem >> sampledSoundChoices [

	self subclassResponsibility
]

{ #category : #'activation hooks' }
SoundSystem >> shutDown [
	"by default do nothing"
]

{ #category : #accessing }
SoundSystem >> soundEnabled [

	^ self class soundEnabled
]

{ #category : #accessing }
SoundSystem >> soundEnabled: aBoolean [

	^ self class soundEnabled: aBoolean
]

{ #category : #query }
SoundSystem >> soundNamed: soundName [

	self subclassResponsibility
]
