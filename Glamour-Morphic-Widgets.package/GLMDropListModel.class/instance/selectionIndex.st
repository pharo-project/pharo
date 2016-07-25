accessing
selectionIndex

	^ selectionIndex ifNil:[ selectionIndex:= self glamourPresentation selectedIndex ]