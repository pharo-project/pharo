Extension { #name : #WorldMorph }

{ #category : #'*Morphic-Base' }
WorldMorph >> acceptDroppingMorph: dropped event: evt [
	"The supplied morph, known to be acceptable to the receiver, is now to be assimilated; the precipitating event is supplied"

	| aMorph |
	aMorph := self morphToDropFrom: dropped.	"Add the given morph to this world and start stepping it if it wants to be."
	self addMorphFront: aMorph.
	(aMorph fullBounds intersects: self viewBox)
		ifFalse: [ 
			self inform: 'Dropped morph not in bounds'.
			aMorph position: self bounds center ].
	aMorph
		submorphsDo: [ :m | 
			(m isKindOf: HaloMorph)
				ifTrue: [ m delete ] ].
	self bringTopmostsToFront
]

{ #category : #'*Morphic-Base' }
WorldMorph >> dispatchKeystroke: anEvent [
	"A keystroke was hit when no keyboard focus was set, so it is sent here to the world instead."

	|  aChar ascii |
	aChar := anEvent keyCharacter.
	(ascii := aChar asciiValue) = 27 ifTrue: "escape key"
		[^ self invokeWorldMenuFromEscapeKey].
	
	(self navigationKey: anEvent) ifTrue: [^self].

	(anEvent commandKeyPressed and: [TextEditor cmdKeysInText])
		ifTrue: [^ self dispatchCommandKeyInWorld: aChar event: anEvent].

	"It was unhandled. Remember the keystroke."
	self lastKeystroke: anEvent keyString.
	self triggerEvent: #keyStroke.
	super dispatchKeystroke: anEvent
]

{ #category : #'*Morphic-Base' }
WorldMorph >> install [

	"this method was used to be invoked from onPrimitiveError: 
	the problem is that it cannot be used to recreate the world instance.
	For this have a look class side installNewWorld."

	owner := nil.	"since we may have been inside another world previously"
	ActiveWorld := self.
	World := self.
	ActiveHand := self hands first.	"default"
	ActiveEvent := nil.
	submorphs do: [ :ss | ss owner ifNil: [ ss privateOwner: self ] ].	"Transcript that was in outPointers and then got deleted."
	self viewBox: Display boundingBox.
	Sensor flushAllButDandDEvents.
	worldState handsDo: [ :h | h initForEvents ].
	self borderWidth: 0.	"default"
	SystemWindow noteTopWindowIn: self.
	self displayWorldSafely
]

{ #category : #'*Morphic-Base' }
WorldMorph >> installForUIProcessReinstall [
	"This method should be changed because it cannot be invoked from onPrimitiveError: as the previous PasteUpMorph 
	method was used to."
	
	| morphs  |
	owner := nil.
	morphs := World submorphs.
	"we should decide if we get morph or not. Now submorphs returns an array and this is 
	strange that this is not an OrderedCollection"
	
	Sensor
		flushNonKbdEvents;
		flushKeyboard;
		flushEvents;
		shutDown.
	self class shutDown.
 
	self addHand: HandMorph new.	
	self borderWidth: 0.
	self viewBox: Display boundingBox.
	self borderWidth: 0.	"default"
	self color: self class defaultWorldColor.
	self addAllMorphs: morphs.
	SystemWindow noteTopWindowIn: self.
	World  := self.
	ActiveWorld := self.
	ActiveHand := self hands first.	"default"
	ActiveEvent := nil.
	worldState handsDo: [ :h | h releaseCachedState ].
	Sensor startUp.
	self class startUp.
	self displayWorldSafely
]

{ #category : #'*Morphic-Base' }
WorldMorph class >> installNewWorld [
	"let us play God."
	
	 [  |  man |
      
        man := MorphicUIManager new.
        "we do not use UIManager default because in headless mode there is not such process."
        man uiProcess ifNotNil: [ man terminateUIProcess  ].
       self new installForUIProcessReinstall.
        man spawnNewProcess.
     
    ] forkAt:  Processor userInterruptPriority
]
