event handling
mouseDown: evt
	self flag: #todo. "This is a quick fix for ensure cmd+click works. 
	see: http://code.google.com/p/pharo/issues/detail?id=7403
	probably a better fix is just remove it, but I want to prevent any possible side effects and 
	there is no time for study the issue as it would be required"
	(evt yellowButtonPressed and: [ evt commandKeyPressed not ]) "First check for option (menu) click"
		ifTrue: [ (self yellowButtonActivity: evt shiftPressed) 
			ifTrue: [ ^ super mouseDown: evt. ]].
	
	"If pane is not empty, pass the event to the last submorph,
	assuming it is the most appropriate recipient (!)"
	scroller hasSubmorphs 
		ifTrue: [ scroller submorphs last mouseDown: (evt transformedBy: (scroller transformFrom: self)) ].
	
	self eventHandler
		ifNotNil: [ self eventHandler mouseDown: evt fromMorph: self ]