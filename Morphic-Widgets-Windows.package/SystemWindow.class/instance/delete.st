open/close
delete
	"Should activate window before asking model if okToChange
	since likely that a confirmation dialog will be requested.
	Don't if not owned by the world though."
	
	| thisWorld animateClose announcement |
	self mustNotClose ifTrue: [^ self].
	"(self owner notNil and: [self owner isWorldMorph])
		ifTrue: [self activate]."
		
	model ifNotNil: [ model okToChange ifFalse: [ ^ self ] ].
	animateClose := (self visible and: [self world notNil and: [
		self theme settings animationSettings useAnimation and: [
		self theme settings animationSettings animateClosing]]]).
	self removePaneSplitters. "in case we add some panes and reopen!"
	thisWorld := self world.
	self isFlexed
		ifTrue: [owner delete]
		ifFalse: [super delete].
	model ifNotNil: [ 
		model 
			windowIsClosing; 
			releaseActionMap ].
	model := nil.
	animateClose ifTrue: [self animateClose].	
	SystemWindow noteTopWindowIn: thisWorld.
	announcement := (WindowClosed new 
						window: self; 
						yourself).
	self announce: announcement.
	World announcer announce: announcement
