Extension { #name : 'WorldMorph' }

{ #category : '*Morphic-Base' }
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

{ #category : '*Morphic-Base' }
WorldMorph >> dispatchKeystroke: anEvent [
	"A keystroke was hit when no keyboard focus was set, so it is sent here to the world instead."

	|  aChar ascii |
	aChar := anEvent keyCharacter.
	(ascii := aChar asciiValue) = 27 ifTrue: "escape key"
		[^ self invokeWorldMenuFromEscapeKey].

	(self navigationKey: anEvent) ifTrue: [^self].

	"It was unhandled. Remember the keystroke."
	self lastKeystroke: anEvent keyString.
	self triggerEvent: #keyStroke.
	super dispatchKeystroke: anEvent
]

{ #category : '*Morphic-Base' }
WorldMorph >> install [

	"This method should not be used. This is for backward compatibility and will be replaced
	with a newer solution"

	owner := nil.	"since we may have been inside another world previously"
	ActiveWorld := self.
	World := self.

	self hands ifEmpty: [self addHand: HandMorph new].

	self worldState worldRenderer: AbstractWorldRenderer mainWorldRenderer.
	AbstractWorldRenderer mainWorldRenderer world: self.

	submorphs do: [ :ss | ss owner ifNil: [ ss privateOwner: self ] ].

	worldState handsDo: [ :h | h initForEvents ].
	self borderWidth: 0.
	SystemWindow noteTopWindowIn: self.
	self displayWorldSafely.
	self extent: self worldState worldRenderer actualScreenSize.
	self viewBox: self worldState worldRenderer viewBox
]

{ #category : '*Morphic-Base' }
WorldMorph >> mouseDown: evt [
	super mouseDown: evt.
	self currentWindow ifNotNil: [ :topWindow | SystemWindow passivateTopWindow ]
]
