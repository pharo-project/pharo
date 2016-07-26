dropping/grabbing
mouseDown: evt
	"Handle a mouse down event."

	| grabbedMorph handHadHalos |
	(self class isMenuOpenByLeftClick
		ifTrue: [ evt yellowButtonPressed "left click" ]
		ifFalse: [ evt redButtonPressed	"right click" ])
		ifTrue: [ ^ self popUpContentsMenu: evt ].
	grabbedMorph := self morphToGrab: evt.
	grabbedMorph
		ifNotNil: [ 
			grabbedMorph isSticky
				ifTrue: [ ^ self ].
			^ evt hand grabMorph: grabbedMorph ].
	(super handlesMouseDown: evt)
		ifTrue: [ ^ super mouseDown: evt ].
	handHadHalos := evt hand halo notNil.
	evt hand removeHalo.	"shake off halos"
	evt hand releaseKeyboardFocus.	"shake of keyboard foci"
	(evt shiftPressed not and: [ self isWorldMorph not and: [ self wantsEasySelection not ] ])
		ifTrue: [ 
			evt wasHandled: false.
			^ self ].	"explicitly ignore the event if we're not the world and we'll not select,
		so that we could be picked up if need be"
	(evt shiftPressed or: [ self wantsEasySelection ])
		ifTrue: [ 
			| clickSelector |
			evt shiftPressed
				ifTrue: [ clickSelector := #popUpContentsMenu: ]
				ifFalse: [ clickSelector := #invokeWorldMenu: ].
			evt hand
				waitForClicksOrDrag: self
				event: evt
				selectors:
					{clickSelector.
					nil.
					nil.
					#dragThroughOnDesktop:}
				threshold: 5 ]
		ifFalse: [ self invokeWorldMenu: evt ]	"We'll select on drag, let's decide what to do on click"	"We wont select, just bring world menu if I'm the world"