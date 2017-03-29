predicate
popup: aPopupBrick event: anEvent
	"Return true when mouse is moved outside of a popup brick, false otherwise"
	<return: #Boolean>
	
	^ (anEvent type = #mouseMove) and: [
		(aPopupBrick isInsideOfPopupBrick: anEvent) not ]