event handling - private
is: anEvent withFocusOver: aMorph 

	| focusedMorph |
	
	focusedMorph := anEvent hand mouseFocus.
	^ aMorph = focusedMorph or: [ aMorph hasOwner: focusedMorph ]