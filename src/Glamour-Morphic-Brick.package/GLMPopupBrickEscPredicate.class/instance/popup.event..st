predicate
popup: aPopupBrick event: anEvent
	"Return true when user presses escape key, false otherwise"
	<return: #Boolean>

	^ (anEvent type == #keystroke) and: [ 
			anEvent keyCharacter = Character escape ]