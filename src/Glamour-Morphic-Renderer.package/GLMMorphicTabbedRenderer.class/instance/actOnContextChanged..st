rendering
actOnContextChanged: ann
	ann property = #activePresentation ifTrue: [ 
		tabs announcer suspendAllWhile: [ tabs selectedPageIndex: (ann presentation matchingPresentations indexOf: ann value) ] ]