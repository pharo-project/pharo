*Morphic-Base
installForUIProcessReinstall
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
	self displayWorldSafely.