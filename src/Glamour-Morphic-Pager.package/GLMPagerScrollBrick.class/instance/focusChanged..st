event-handling
focusChanged: anEvent
	| pageIndex newFocus |
	
	newFocus := anEvent morph.
	
	currentFocus = newFocus ifFalse: [
		currentFocus := newFocus.
		pageIndex := self pageOf: newFocus.
		(pageIndex > 0) ifTrue: [
			pagerModel focusedMorphChanged: newFocus at: pageIndex ] ]