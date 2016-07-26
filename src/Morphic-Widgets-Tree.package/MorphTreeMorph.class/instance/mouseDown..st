events-processing
mouseDown: event 
	"Changed to take keybaord focus."
	| targetMorph selectors |
	self enabled ifFalse: [ ^self ].
	mouseOverAllowed := true.
	self wantsKeyboardFocus
		ifTrue: [self takeKeyboardFocus].
	(event yellowButtonPressed and: [(self commandOrCrontrolKeyPressed: event) not])
		ifTrue: ["First check for option (menu) click"			
			^ self yellowButtonEvent: event].
	(targetMorph := self scrollerSubMorphFromPoint: event position)
		ifNotNil: [targetMorph hasToggle
				ifTrue: [(targetMorph
							inToggleArea: (targetMorph point: event position from: self))
						ifTrue: [^ self toggleExpandedState: targetMorph event: event]]].
	targetMorph
		ifNil: [^ super mouseDown: event].
	targetMorph highlightForMouseDown.
	selectors := Array
				with: #click:
				with: #doubleClick:
				with: nil
				with: (self dragEnabled
						ifTrue: [#startDrag:]).
	event hand
		waitForClicksOrDrag: self
		event: event
		selectors: selectors
		threshold: 10.

	self listManager mouseDown: event on: targetMorph

