event handling
mouseDown: evt
	"Changed to take keybaord focus."
	
	| aMorph selectors |
	self wantsKeyboardFocus
		ifTrue: [self takeKeyboardFocus].
	aMorph := self itemFromPoint: evt position.
	(aMorph notNil and:[aMorph inToggleArea: (aMorph point: evt position from: self)])
		ifTrue:[^self toggleExpandedState: aMorph event: evt]. 
	
	evt yellowButtonPressed  "First check for option (menu) click"
		ifTrue: [ 
			(self yellowButtonActivity: evt shiftPressed) 
				ifTrue: [ ^ self ]].
		
	aMorph ifNil:[^super mouseDown: evt].
	aMorph highlightForMouseDown.
	selectors := Array 
		with: #click:
		with: nil
		with: nil
		with: (self dragEnabled ifTrue:[#startDrag:] ifFalse:[nil]).
	evt hand waitForClicksOrDrag: self event: evt selectors: selectors threshold: 10 "pixels".