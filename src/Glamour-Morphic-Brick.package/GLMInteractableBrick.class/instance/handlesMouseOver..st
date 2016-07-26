brick-morph-mouse-events
handlesMouseOver: evt
	^ self isEnabled
		ifTrue: [ true ]
		ifFalse: [ super handlesMouseDown: evt ]