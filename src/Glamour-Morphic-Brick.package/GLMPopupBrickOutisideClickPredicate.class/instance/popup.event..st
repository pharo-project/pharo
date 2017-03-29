predicate
popup: aPopupBrick event: anEvent
	"Return true when user clicks outside of a popup brick, false otherwise"
	<return: #Boolean>

	^ (anEvent type = #mouseDown) and: [
		(aPopupBrick isInsideOfPopupBrick: anEvent) not ]